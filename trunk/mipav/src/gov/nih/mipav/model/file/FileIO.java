package gov.nih.mipav.model.file;


import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.dicomcomm.DICOM_Constants;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.file.FileInfoDicom.VRtype;
import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.provenance.actions.ActionOpenImage;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.actions.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.image.*;
import java.io.*;
import java.lang.management.ManagementFactory;
import java.util.*;
import java.util.zip.*;

import javax.imageio.ImageIO;
import javax.swing.*;

import com.sun.jimi.core.*;


/**
 * This class controls the file input/output of most formats that MIPAV supports, including tiff, raw, analyze, DICOM,
 * and Medvision. It switches based on file type and calls the file constructors, readers, and writers for the specific
 * file type. However, note the FileAvi and FileQT are called directly from ViewJFrameBase for file writes.
 * 
 * @version 0.1 Sept 5, 1997
 * @author Matthew J. McAuliffe, Ph.D.
 * @author Neva Cherniavsky
 * @see FileAnalyze
 * @see FileDicom
 * @see FileMedVision
 * @see FileRaw
 * @see FileTiff
 */
public class FileIO {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private static final String FILE_READ = "Opening ";

    /** DOCUMENT ME! */
    private static final String FILE_WRITE = "Saving ";

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Directory where the image file can be found. */
    private String fileDir;

    /** If a LUT is to be saved with the image it is stored here. */
    private ModelLUT LUT = null;

    /** DOCUMENT ME! */
    private ModelRGB modelRGB = null;

    /** DOCUMENT ME! */
    private ViewJProgressBar progressBar = null;

    /** Refers to whether or not the FileIO reports progress or errors. */
    private boolean quiet = false;

    /** DOCUMENT ME! */
    private RawImageInfo rawInfo = null;

    /** Address of second image in file. */
    private int secondImage = 0;

    /** Reference to the user interface. */
    private final ViewUserInterface UI;

    /**
     * Dialog to prompt the user to determine the correct file type. If the type of file to read is unknown (ie., the
     * suffix doesn't match one of the known types, build and display the unknown file dialog so the user can try to
     * identify the image type so the correct reader can be used
     */
    private JDialogUnknownIO unknownIODialog = null;

    // here for now....11/13/2008
    private boolean saveAsEncapJP2 = false;

    private boolean displayRangeOfSlicesDialog = true;

    /** For multi file formats where data is saved in a set of files */
    private String[] dataFileName = null;
    
    private DTIParameters dtiparams;

    private boolean isDTISort = false;
    
    

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates the FileIO and displays
     * <q>Choose File Type</q>
     * unknown file dialog. Constructs a new FileIO object, sets the user interface, and initializes the unknown file
     * dialog. Also gets the userDefinedFileTypeAssociations preferences
     */
    public FileIO() {
        UI = ViewUserInterface.getReference();

        if ( !GraphicsEnvironment.isHeadless()) {
            unknownIODialog = new JDialogUnknownIO(UI.getMainFrame(), "Choose File Type");
        }
        UI.setLoad(false); // default to "opening "
    }

    /**
     * Creates the FileIO and displays
     * <q>Choose File Type</q>
     * unknown file dialog. Constructs a new FileIO object, sets the user interface, sets the LUT, and initializes the
     * unknown file dialog. Also gets the userDefinedFileTypeAssociations preferences
     * 
     * @param _LUT Passes LUT into file IO object so that LUT can be store with image (i.e. TIFF).
     */
    public FileIO(final ModelLUT _LUT) {
        UI = ViewUserInterface.getReference();
        LUT = _LUT;

        if ( !GraphicsEnvironment.isHeadless()) {
            unknownIODialog = new JDialogUnknownIO(UI.getMainFrame(), "Choose File Type");
        }
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Sets specific types to be multifile based on the input argument. Not all file types are supported to handle
     * multifiles. Those that do support building a 3D image out of a series of 2D images are defined to have that input
     * capability here.
     * 
     * @param fileType Input file type. File types which MIPAV does not allow multifile images get the file type
     *            returned as sent.
     * @param multiFile If <code>true</code>, returns a new filetype corresponding to the input filetype + MULTIFILE.
     *            If <code>false</code>, returns the given filetype.
     * 
     * @return The new or old fileType.
     */
    public static final int chkMultiFile(final int fileType, final boolean multiFile) {
        int fType = fileType;

        if (multiFile) {

            if (fileType == FileUtility.TIFF) {
                fType = FileUtility.TIFF_MULTIFILE;
            } else if (fileType == FileUtility.LSM) {
                fType = FileUtility.LSM_MULTIFILE;
            } else if (fileType == FileUtility.DICOM) {
                fType = FileUtility.DICOM;
            } // affords some posibilities
            else if (fileType == FileUtility.ANALYZE) {
                fType = FileUtility.ANALYZE_MULTIFILE;
            } // under construction
            else if (fileType == FileUtility.NIFTI) {
                fType = FileUtility.NIFTI_MULTIFILE;
            } else if (fileType == FileUtility.RAW) {
                fType = FileUtility.RAW_MULTIFILE;
            } // under construction
            else if (fileType == FileUtility.COR) {
                fType = FileUtility.COR;
            } else if (fileType == FileUtility.XML) {
                fType = FileUtility.XML_MULTIFILE;
            } else if (fileType == FileUtility.GE_SIGNA4X) {
                fType = FileUtility.GE_SIGNA4X_MULTIFILE;
            } else if (fileType == FileUtility.GE_GENESIS) {
                
                fType = FileUtility.GE_GENESIS_MULTIFILE;
            } else if (fileType == FileUtility.MAGNETOM_VISION) {
                fType = FileUtility.MAGNETOM_VISION_MULTIFILE;
            } else if (fileType == FileUtility.BMP){
                fType = FileUtility.BMP_MULTIFILE;
            }
        }

        return fType;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param modelImage ModelImage - the image to resample to UBYTE
     * 
     * @return ModelImage
     */
    public static final ModelImage convertToARGB(final ModelImage modelImage) {
        final float min = (float) modelImage.getMin();
        final float max = (float) modelImage.getMax();

        float[] oneSliceBuffer = new float[modelImage.getExtents()[0] * modelImage.getExtents()[1] * 4];

        final ModelImage modelImageResultARGB = new ModelImage(ModelStorageBase.ARGB, modelImage.getExtents(),
                modelImage.getImageName());

        try {

            for (int i = 0; i < modelImage.getExtents()[2]; i++) // loop through images
            {
                modelImage.exportData(i * oneSliceBuffer.length, oneSliceBuffer.length, oneSliceBuffer); // export a
                // 2d buffer
                // from
                // modelImageResult

                oneSliceBuffer = FileIO.resample255(oneSliceBuffer, min, max);

                modelImageResultARGB.importData(i * oneSliceBuffer.length, oneSliceBuffer, false); // import into
                // modelImageResultUB

                FileIO.copyResolutions(modelImage, modelImageResultARGB, i);
            }

            modelImageResultARGB.calcMinMax();
        } catch (final Exception e) {
            Preferences.debug(e.getMessage(), Preferences.DEBUG_MINOR);
        } finally {
            modelImage.disposeLocal(false);
        }

        return modelImageResultARGB;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param modelImageResult ModelImage - the image to resample to UBYTE
     * 
     * @return ModelImage
     */
    public static final ModelImage convertToUBYTE(final ModelImage modelImageResult) {
        final float min = (float) modelImageResult.getMin();
        final float max = (float) modelImageResult.getMax();
        float[] oneSliceBuffer = new float[modelImageResult.getExtents()[0] * modelImageResult.getExtents()[1]];

        final ModelImage modelImageResultUB = new ModelImage(ModelStorageBase.UBYTE, modelImageResult.getExtents(),
                modelImageResult.getImageName());

        try {
            if (modelImageResult.getNDims() > 2) {
                for (int i = 0; i < modelImageResult.getExtents()[2]; i++) // loop through images
                {
                    modelImageResult.exportData(i * oneSliceBuffer.length, oneSliceBuffer.length, oneSliceBuffer); // export
                    // a 2d
                    // buffer
                    // from
                    // modelImageResult

                    oneSliceBuffer = FileIO.resample255(oneSliceBuffer, min, max);

                    modelImageResultUB.importData(i * oneSliceBuffer.length, oneSliceBuffer, false); // import into
                    // modelImageResultUB

                    FileIO.copyResolutions(modelImageResult, modelImageResultUB, i);
                }
            } else {
                // 2d image
                modelImageResult.exportData(0, oneSliceBuffer.length, oneSliceBuffer);
                oneSliceBuffer = FileIO.resample255(oneSliceBuffer, min, max);
                modelImageResultUB.importData(0, oneSliceBuffer, false);
                FileIO.copyResolutions(modelImageResult, modelImageResultUB, 0);
            }

            modelImageResultUB.calcMinMax();
        } catch (final Exception e) {
            Preferences.debug(e.getMessage(), Preferences.DEBUG_MINOR);
        } finally {
            modelImageResult.disposeLocal(false);
        }

        return modelImageResultUB;
    }

    /**
     * The purpose of this method is to subsample a ModelImage to the dimensions specified by the subsampleDimension
     * parameter.
     * 
     * @param modelImage ModelImage - the image to be subsampled. This image will be destroyed in the course of the
     *            algorithm
     * @param subsampleDimension Dimension - the dimensions to subsample to.
     * 
     * @return ModelImage - the subsampled image
     */
    public static final ModelImage subsample(final ModelImage modelImage, final Dimension subsampleDimension) {
        final int[] subsampledExtents = new int[] {subsampleDimension.getSize().width,
                subsampleDimension.getSize().height};
        // SubSample dialog now allows users to pad image so that extents are divisible by the subsampling scalar.
        final int[] padExtents = modelImage.getExtents();
        final ModelImage modelImageResult = new ModelImage(modelImage.getType(), new int[] {
                subsampleDimension.getSize().width, subsampleDimension.getSize().height}, modelImage.getImageName()
                + "_subsampled");

        final AlgorithmSubsample algorithmSubsample = new AlgorithmSubsample(modelImage, modelImageResult,
                subsampledExtents, padExtents, new float[] {1.0f, 1.0f, 1.0f}, false, false, null, false);
        algorithmSubsample.run();

        modelImage.disposeLocal(false);

        return modelImageResult;
    }

    /**
     * Presents a dialog for a user-entered definition of the image type.
     * 
     * @return The image file type entered by the user, or FileUtility.ERROR if FileIO is quiet or the dialog is
     *         cancelled. see FileUtility.ERROR see isQuiet()
     * 
     * @see JDialogUnknownIO
     */
    public int getFileType() {
        int fileType;

        if (quiet) {
            return FileUtility.ERROR;
        }

        unknownIODialog.setVisible(true);

        if (unknownIODialog.isCancelled()) {
            fileType = FileUtility.ERROR;
        } else {
            fileType = unknownIODialog.getFileType();
        }

        return fileType;
    }

    /**
     * Returns LUT associated with the image file.
     * 
     * @return The LUT associated with the image although it may be null.
     */
    public ModelLUT getModelLUT() {
        return LUT;
    }

    /**
     * Gets the model RGB.
     * 
     * @return ModelRGB
     */
    public ModelRGB getModelRGB() {
        return modelRGB;
    }

    /**
     * Returns address of TIFF header of second image if present in CZ-Private Tag of LSM 510 file Returns zero if not
     * present.
     * 
     * @return secondImage
     */
    public int getSecondImage() {
        return secondImage;
    }

    /**
     * Refers to whether or not the FileIO will send alerts to the user about progress or errors.
     * 
     * @return Whether to suppress GUI elements which would require user interaction (ie, error dialogs).
     */
    public boolean isQuiet() {
        return quiet;
    }

    /**
     * Reads a list of DICOM files. DICOM images have all their slices in separate files (except multi-frame), with
     * different information in each header. The position of the slice in the image is determined by information found
     * in the header so all the headers must be read before the images can be read. That's why this method goes through
     * all the images twice.
     * 
     * @param selectedFileName Name of the image file selected to be readin <code>this#fileDir</code> to read. Used to
     *            ID study and series number
     * @param fileList List of all the files to be read in.
     * @param performSort <code>true</code> if this method is to sort the files in the list, or, <code>
     *                            false</code>
     *            will apply the images in each file in the order they appear as the order to use in the ModelImage.
     * 
     * @return The image that was read in, or null if failure.
     */
    public ModelImage readDicom(String selectedFileName, String[] fileList, final boolean performSort) {

        FileDicom imageFile;
        FileInfoDicom refFileInfo;
        FileInfoDicom[] savedFileInfos;

        float[] bufferFloat = null;
        short[] bufferShort = null;
        int[] bufferInt = null;
        int[] extents;
        int length = 0;
        int nImages = 0;
        int nListImages;
        
        int dtiSliceCounter = -1;
        String studyDescription = null;
        String seriesDescription = null;
        String scannerType = null;
        
        final float[] tPt = new float[3];
        TransMatrix matrix = null;
        String studyID = new String();
        String seriesNo = new String();
        String acqNo = new String();
        String seriesNoRef = new String();
        String studyIDMaster;
        String seriesNoMaster;
        String acqNoMaster;

        FileDicomTagTable[] childrenTagTables;

        int orientation = FileInfoBase.UNKNOWN_ORIENT;

        if (fileList.length == 0) {
            return null;
        }
        // System.err.println("quiet: " + quiet);
        /*
         * System.out.println("selectedFileName = " + selectedFileName); for (int m = 0; m < fileList.length; m++) {
         * System.out.println("Filelist = " + m + " " + fileList[m]);}
         */

        try {
            nListImages = fileList.length;

            savedFileInfos = new FileInfoDicom[nListImages];

            // use the selectedFileName as the reference slice for the file info tag tables
            imageFile = new FileDicom(selectedFileName, fileDir);
            imageFile.setQuiet(quiet); // if we want quiet, we tell the reader, too.
            createProgressBar(null, FileUtility.trimNumbersAndSpecial(selectedFileName)
                    + FileUtility.getExtension(selectedFileName), FileIO.FILE_READ);
            imageFile.readHeader(true); // can we read the header?
            final String modality = getModality(imageFile);
            if (modality != null && modality.equals("SR")) {
                // TODO:Structured report handling would be implemented here (since the rest of this method reads the
                // image file
                removeFromImageList(selectedFileName, fileList);
                if (fileList.length == 0) {
                    if ( !quiet) {
                        MipavUtil.displayInfo("MIPAV cannot process this structured report DICOM file.");
                    }
                    return null;
                } else {
                    Preferences.debug(selectedFileName + " contained unreadable DICOM data", Preferences.DEBUG_FILEIO);
                    selectedFileName = fileList[0];
                }
                return readDicom(selectedFileName, fileList, performSort);
            }
        } catch (final OutOfMemoryError error) {

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            }

            error.printStackTrace();

            return null;
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            }

            error.printStackTrace();

            return null;
        }

        refFileInfo = (FileInfoDicom) imageFile.getFileInfo();
        savedFileInfos[0] = refFileInfo;

        try {

            // image2d = new ModelImage(myFileInfo.getDataType(), myFileInfo.getExtents(), UI);
            if (ModelImage.isColorImage(refFileInfo.getDataType())) { // / other type of ARGB
                length = refFileInfo.getExtents()[0] * refFileInfo.getExtents()[1] * 4;
            } else {
                length = refFileInfo.getExtents()[0] * refFileInfo.getExtents()[1];
            }

            // if length is 0...this means that extents were not set...and this is becasue..in the case
            // of dicom spectroscopy images, the image data is not under FileDicom.IMAGE_TAG
            if (length == 0) {
                MipavUtil.displayError("Unable to open DICOM Spectroscopy image");
                return null;
            }

            // TODO: should both of these always be allocated?
            bufferFloat = new float[length];
            if (refFileInfo.getDataType() == ModelStorageBase.UINTEGER) {
                bufferInt = new int[length];
            } else {

                bufferShort = new short[length];
            }
        } catch (final OutOfMemoryError error) {
            bufferFloat = null;
            bufferInt = null;
            bufferShort = null;
            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            }

            error.printStackTrace();

            return null;
        }

        // look for number of frames tag (0028,0008) != null && > 1 nImages = number of frames
        if (refFileInfo.getTagTable().getValue("0028,0008") != null) {
            nImages = Integer.valueOf( ((String) (refFileInfo.getTagTable().getValue("0028,0008"))).trim()).intValue();

            if (nImages > 1) {
                refFileInfo.multiFrame = true;

            }

            Preferences.debug("0028,0008 (nImages) == " + nImages + "\n", Preferences.DEBUG_FILEIO);
        }

        try {

            if (refFileInfo.getTagTable().getValue("0020,0010") != null) {
                studyIDMaster = (String) (refFileInfo.getTagTable().getValue("0020,0010"));
                studyIDMaster = studyIDMaster.trim();
            } else {
                studyIDMaster = "";
            }

            if (refFileInfo.getTagTable().getValue("0020,0012") != null) {
                acqNoMaster = (String) (refFileInfo.getTagTable().getValue("0020,0012"));
                acqNoMaster = acqNoMaster.trim();
            } else {
                acqNoMaster = "";
            }

            if (refFileInfo.getTagTable().getValue("0020,0011") != null) {
                seriesNoMaster = (String) (refFileInfo.getTagTable().getValue("0020,0011"));
                seriesNoRef = (String) (refFileInfo.getTagTable().getValue("0020,0011"));
                seriesNoMaster = seriesNoMaster.trim();

                if (seriesNoRef.length() > 5) {
                    seriesNoRef = seriesNoMaster.substring(0, Math.min(5,seriesNoMaster.length()));
                }
            } else {
                seriesNoMaster = "";
                seriesNoRef = "";
            }

        } catch (final OutOfMemoryError error) {

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            }

            error.printStackTrace();

            return null;
        }

        int[] indices = null;
        final int[] orient = new int[3]; // for FileInfoBase values. eg:FileInfoBase.ORI_S2I_TYPE;
        int pBarVal = 0;
        
        boolean validInstanceSort = false, validOriSort = false, validOri4DSort = false;

        if ( !refFileInfo.isMultiFrame()) {

            /*
             * this code is for setting the fixed axis in the 3-D image needed for displaying overlay information
             * properly we look at the first two images and see which position varies by the third resolution. that
             * position indicates which axis the image is sliced on, which indicates the orientation first go through
             * headers and find out where in the array to store the image
             */
            // files are loaded into the appropriate place in the image
            // buffer as indicated by the slice numbers provided in indicies.
            // the values are set to the values of either zOri or rint (after
            // the sorting has been done) as needed, depending on sorting.
            indices = new int[nListImages];

            final int[] zOri = new int[nListImages]; // sorted image orientation values (ie., image z-axis)
            final int[] zOri4D = new int[nListImages]; //sorted image orientation values taking into account overlapping time zones
            final int[] zInst = new int[nListImages]; //sorted image instance values
            final float[] zOrients = new float[nListImages]; // image orientation values as read in.
            final float[] instanceNums = new float[nListImages]; // image instance numbers as read in.

            // progressBar.setTitle("Reading headers");

            nImages = 0;

nList:      for (int i = 0; i < nListImages; i++) {

                try {

                    if ( ((float) i / (nListImages - 1) * 100) > pBarVal) {
                        pBarVal += 10;
                        progressBar.updateValue(Math.round((float) i / (10 * (nListImages - 1)) * 100), false);
                    }

                    FileInfoDicom fileInfoTemp;

                    if ( !fileList[i].equals(selectedFileName)) {
                        imageFile.setFileName(fileList[i], refFileInfo);
                        imageFile.readHeader(true);
                        fileInfoTemp = (FileInfoDicom) imageFile.getFileInfo();
                    } else {
                        fileInfoTemp = refFileInfo;
                    }

                    final String modality = getModality(imageFile);
                    if (modality != null && modality.equals("SR")) {
                        // TODO:Structured report handling would be implemented here (since the rest of this method
                        // reads the image file
                        fileList = removeFromImageList(selectedFileName, fileList);
                        if (fileList.length == 0) {
                            if ( !quiet) {
                                MipavUtil.displayInfo("MIPAV cannot process this structured report DICOM file.");
                            }
                            return null;
                        } else {
                            Preferences.debug(selectedFileName + " contained unreadable DICOM data",
                                    Preferences.DEBUG_FILEIO);
                            selectedFileName = fileList[0];
                        }
                        return readDicom(selectedFileName, fileList, performSort);
                    }

                    // If study and series number match - Continue.  If these weren't equal MIPAV will not be able to handle this
                    if (fileInfoTemp.getTagTable().getValue("0020,0010") != null) {
                        studyID = (String) (fileInfoTemp.getTagTable().getValue("0020,0010"));
                        studyID = studyID.trim();
                    }

                    if (fileInfoTemp.getTagTable().getValue("0020,0011") != null) {
                        seriesNo = (String) (fileInfoTemp.getTagTable().getValue("0020,0011"));
                        seriesNo = seriesNo.trim();
                    }

                    if (fileInfoTemp.getTagTable().getValue("0020,0012") != null) {
                        acqNo = (String) (fileInfoTemp.getTagTable().getValue("0020,0012"));
                        acqNo = acqNo.trim();
                    }
                    
                    //if dimensions match - Continue;
                    if(refFileInfo.getExtents() != null && fileInfoTemp.getExtents() != null) {
                        for(int j=0; j<fileInfoTemp.getExtents().length; j++) {
                            if(fileInfoTemp.getExtents()[j] != refFileInfo.getExtents()[j]) {
                                fileList = removeFromImageList(fileList[i], fileList);
                                continue nList;
                            }
                        }
                    }

                    if (performSort) {
                        if (seriesNo.equals(seriesNoMaster) && studyID.equals(studyIDMaster)) { // &&
                            // acqNo.equals(acqNoMaster))
                            // {
                            savedFileInfos[nImages] = fileInfoTemp;

                            // this matrix is the matrix that converts this image into
                            // a standard DICOM axial image
                            matrix = savedFileInfos[nImages].getPatientOrientation();

                            if (matrix != null) {

                                /*
                                 * transform the x location, y location, and z location, found from the Image Position
                                 * tag, by the matrix. The tPt array now has the three numbers arranged as if this image
                                 * had been transformed. The third place in the array holds the number that the axis is
                                 * being sliced along. xlocation, ylocation, zlocation are from the DICOM tag 0020,0032
                                 * patient location;
                                 */
                                matrix.transform(savedFileInfos[nImages].xLocation, savedFileInfos[nImages].yLocation,
                                        savedFileInfos[nImages].zLocation, tPt);

                                // tPt[2] is MIPAV's z-axis. It is the position of the patient
                                // along the axis that the image was sliced on.
                                zOrients[nImages] = tPt[2];
                            } else {
                                zOrients[nImages] = 1;
                            }

                            // instance numbers used in case improper DICOM and position and orientation info not given
                            instanceNums[nImages] = savedFileInfos[nImages].instanceNumber;
                            zOri[nImages] = nImages;
                            zInst[nImages] = nImages;
                            nImages++;
                        }
                    } else {
                        savedFileInfos[nImages] = fileInfoTemp;
                        matrix = savedFileInfos[nImages].getPatientOrientation();

                        if (matrix != null) {
                            matrix.transform(savedFileInfos[nImages].xLocation, savedFileInfos[nImages].yLocation,
                                    savedFileInfos[nImages].zLocation, tPt);
                            zOrients[nImages] = tPt[2];
                        } else {
                            zOrients[nImages] = 1;
                        }

                        // instance numbers used in case improper DICOM and position and orientation info not given
                        instanceNums[nImages] = savedFileInfos[nImages].instanceNumber;
                        zOri[nImages] = nImages;
                        zInst[nImages] = nImages;
                        nImages++;
                    }
                } catch (final IOException error) {

                    if ( !quiet) {
                        MipavUtil.displayError("FileIO: " + error);
                        Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug(" FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
                    }

                    error.printStackTrace();

                    System.gc();

                    return null;
                }

            }
            
            if(fileList.length != savedFileInfos.length) {
                progressBar.dispose();
                return readDicom(selectedFileName, fileList, performSort); //fileList has been modified, read dicom based on this new file list
            }
            

            // need to let the reference tag table know about the tag tables which refer to it
            childrenTagTables = new FileDicomTagTable[savedFileInfos.length - 1];

            for (int i = 0, j = 0; i < savedFileInfos.length; i++) {
                if (savedFileInfos[i] != refFileInfo && childrenTagTables.length != 0) {
                    childrenTagTables[j] = savedFileInfos[i].getTagTable();
                    j++;
                }
            }

            refFileInfo.getTagTable().attachChildTagTables(childrenTagTables);

            if (matrix != null) {
                Preferences.debug("Dicom matrix = \n" + matrix + "\n", Preferences.DEBUG_FILEIO);
            }

            /*
             * if this method was told to, we will sort in more than one way: we will first try to order the image set
             * based on orientation of slices. If there are slices in the image set which have the same location on the
             * Z-axis, the data may be a time-based set. If we guess that it is (and there aren't nearly enough
             * test-datasets), we attempt re-order the ordered Z-locations based on instance number to pull out the
             * time-data (ie., of two images with the same Z-axis, a lower instance number was taken at an earlier time;
             * all such earlier-time images are grouped). If neither solution seemed to work, then we order all the
             * images strictly on the instance number. There might be some problems if we had to do this. If the list
             * sent to this method was found using the getFileList() method, all the names are in lexicographical order.
             * This is the default ordering if neither sorting method works, or if performSort is false.
             */
            

            if (performSort) {
                
             // sort so that instance numbers are in ascending order.
                // rint is the index to associate input file-list with the
                // instance number  
                if (!(validInstanceSort = FileIO.sort(instanceNums, zInst, nImages))) {
                    Preferences.debug("FileIO: instance numbers sort failed\n", Preferences.DEBUG_FILEIO);
                    System.err.println("FileIO: instance numbers sort failed on " + fileList[0]);
                } else {
                    
                    refFileInfo = (FileInfoDicom) imageFile.getFileInfo();
                    FileDicomTagTable tagTable =  refFileInfo.getTagTable();
                    studyDescription = (String) tagTable.getValue("0008,1030");
                    seriesDescription = (String) tagTable.getValue("0008,103E");
                    scannerType = (String) tagTable.getValue("0008,0070");
    
                    if ((studyDescription != null && studyDescription.toUpperCase().contains("DTI")) || 
                        (seriesDescription != null && seriesDescription.toUpperCase().contains("DTI"))) {
                        //sortDti sets indices values
                        dtiSliceCounter = sortDtiDicomData(studyDescription, seriesDescription, scannerType, tagTable, savedFileInfos, indices, instanceNums.length, zInst);
                    } 
                }
                
                //always try to generate validOriSort, if a validOriSort that trumps a validInstanceSort

                if ( (nImages > 1 && !isDTISort)) {// && !validInstanceSort) {
                    // sort so that zOrients is now in ascending order.
                    // zOri[i] represents where in the image buffer image
                    // number i should be stored; so that if the images were
                    // read in 1, 10, 11... (which happens often),
                    // zOri[1] = 1 but zOri[2] = 2, rather than 10.
                    if (!(validOriSort = FileIO.sort(zOrients, zOri, nImages))) {
                        Preferences.debug("FileIO: orientation number sort failed\n", Preferences.DEBUG_FILEIO);
                        System.err.println("FileIO: instance number sort failed on " + fileList[0]);
                        
                        // Follow-on ordering if not a valid orientation sort:
                        // pre-order the orientation numbers to *****match the instance
                        // numbers**** (this is not a valid way to do 4D ordering). This is done to accomodate 4D dicom sets.
                        // To describe the algo: I and L value lists which are
                        // independant of each other, and M, which is a copy of L.
                        // a & b are index lists for I and L, respectivly.
                        // L[b[z]] = M[a[z]]
                        // we copy the values of M, using order a, into L, using
                        // order b, thereby making L mimic I.
                        // we do this here with zOrients/zOri to make zOrient match
                        // the ordering of the Instance numbers.
                        final float[] lima = new float[zOrients.length]; // temp

                        System.arraycopy(zOrients, 0, lima, 0, zOrients.length);

                        for (int z = 0; z < nImages; z++) {
                            zOrients[zOri[z]] = lima[zInst[z]]; // copy z-location
                        }

                        for(int i=0; i<zInst.length; i++) {
                            zOri4D[i] = zInst[i]; // copy the indexing
                        }
                            
                        FileIO.sort(zOrients, zOri4D, nImages); // now sort by orientation

                        // Rely on ******image instance number******* (this is not a valid way to do 4D ordering) and position information.
                        // Build a list for all images at a particular location,
                        // although at different times, as judged by ******image instance******.
                        // Create a list for all possible times in the imageset:
                        Vector<Vector<OrientStatus>> timezonesList = new Vector<Vector<OrientStatus>>();

                        // Hold the original list of orients and indices:
                        Vector<OrientStatus> orientsList = new Vector<OrientStatus>(nImages); // original index list

                        for (int k = 0; k < nImages; k++) { // load original list vector

                            final OrientStatus oriReference = new OrientStatus(zOri4D[k], zOrients[k]);

                            orientsList.add(oriReference);
                        }

                        // each times list has a list of the images of different
                        // locations taken at the same time (in the same time-zone)
                        // we check on different times by going through the list of
                        // images and looking for the next lowest image instance
                        // with the same z location. essentially, we
                        // pass through the orientation list multiple times,
                        // filling a single zone with each pass.
                        Vector<OrientStatus> tz;
                        OrientStatus ref0, refi;

                        while (orientsList.size() > 0) {
                            tz = new Vector<OrientStatus>(); // create a new timezone
                            ref0 = orientsList.remove(0);
                            tz.add(ref0); // remove 1st orient, put in timezone
                            Preferences.debug("Loading, and making comparison to: " + ref0.getIndex() + ".."
                                    + ref0.getLocation() + "\n", Preferences.DEBUG_FILEIO);
                            Vector<OrientStatus> orientsClone = (Vector<OrientStatus>) orientsList.clone();

                            for (final Enumeration<OrientStatus> e = orientsClone.elements(); e.hasMoreElements();) {
                                refi = e.nextElement();
                                Preferences.debug("Looking at: " + refi.getIndex() + "..." + refi.getLocation(),
                                        Preferences.DEBUG_FILEIO);

                                if ( !refi.equals(ref0)) {
                                    ref0 = null;
                                    ref0 = refi;
                                    tz.add(refi);
                                    Preferences.debug("...Accepting ", Preferences.DEBUG_FILEIO);

                                    if (orientsList.remove(refi)) {
                                        Preferences.debug(" .... Successfully removed " + refi.getIndex(),
                                                Preferences.DEBUG_FILEIO);
                                    }

                                    Preferences.debug("!!  Comparison to: " + ref0.getIndex() + ".." + ref0.getLocation()
                                            + "\n", Preferences.DEBUG_FILEIO);

                                } else {
                                    Preferences.debug("\n", Preferences.DEBUG_FILEIO);
                                }
                            }

                            orientsClone.clear();
                            orientsClone = null;
                            timezonesList.add(tz);
                            tz = null;
                        }

                        orientsList.clear();
                        orientsList = null;

                        // new ordering for all timezones.
                        try {
                            int t = 0;

                            for (final Vector<OrientStatus> vector : timezonesList) {
                                Preferences.debug("Timezone\n", Preferences.DEBUG_FILEIO);

                                for (final OrientStatus ref : vector) {
                                    final int k = ref.getIndex();

                                    // concatenate the sublists into one ordering list.
                                    zOri4D[k] = t;
                                    zOrients[k] = ref.getLocation();
                                    Preferences.debug("reordering: (" + k + "): " + zOri4D[k] + "..." + zOrients[k] + "\n",
                                            Preferences.DEBUG_FILEIO);
                                    t++;
                                }
                            }

                            Preferences.debug("4D, translation passes!  " + t + " slices!\n", Preferences.DEBUG_FILEIO);
                        } catch (final ArrayIndexOutOfBoundsException enumTooFar) {
                            Preferences.debug("NOT 4D, and DICOM translation fail!\n", Preferences.DEBUG_FILEIO);
                        }

                        timezonesList.clear();
                        timezonesList = null;
                        validOri4DSort = true;
                        System.gc();
                    } else {
                        Preferences.debug("Valid orientation sort found\n", Preferences.DEBUG_FILEIO);
                    }
                        
                } // end of oriSort
                    
                    

            } // end of performSort

            // A separate and equally valid explanation of the Image Orientation matrix is: X patient Y patient Z
            // patient X image 1 0 0 Y image 0 1 0 Z image 0 0 1
            // In this case, the number "1" at X image, X patient means the x in the image is the same as the x in the
            // patient and goes in the same direction; thus, the image x goes from right to left. If it were a "-1",
            // that would mean the image x goes from left to right. If it were a "0", and there was a "1" in X image, Y
            // patient, that would mean that the image x goes from the anterior of the patient to the posterior. This
            // is the case in sagittal images, for example.
            //
            // We're concerned with what the z image is, because that tells us what the orientation is. If there is a
            // "1"
            // in X patient, the patient x is the image's z-axis, making this a sagittal image. If there is a "1" in Y
            // patient, the patient y is the image's z-axis, making this a coronal image. And if there is a "1" in Z
            // patient, the patient z is the image's z-axis, making this an axial image. We look at absolute value
            // because here we are not concerned about right to left vs left to right, only which patient axis the
            // slices were taken along.
            float xCos = 0, yCos = 0, zCos = 0;

            // if (matrix != null && valid == true) {
            if (matrix != null) {
                // get back to original matrix
                xCos = Math.abs(matrix.get(2, 0));
                yCos = Math.abs(matrix.get(2, 1));
                zCos = Math.abs(matrix.get(2, 2));

                for (int j = 0; j < orient.length; j++) {
                    final int index = absBiggest(matrix.get(j, 0), matrix.get(j, 1), matrix.get(j, 2));

                    if (index == 0) {

                        if (matrix.get(j, 0) > 0) {
                            orient[j] = FileInfoBase.ORI_R2L_TYPE;
                        } else {
                            orient[j] = FileInfoBase.ORI_L2R_TYPE;
                        }
                    } else if (index == 1) {

                        if (matrix.get(j, 1) > 0) {
                            orient[j] = FileInfoBase.ORI_A2P_TYPE;
                        } else {
                            orient[j] = FileInfoBase.ORI_P2A_TYPE;
                        }
                    } else { // index == 2

                        if (matrix.get(j, 2) > 0) {
                            orient[j] = FileInfoBase.ORI_I2S_TYPE;
                        } else {
                            orient[j] = FileInfoBase.ORI_S2I_TYPE;
                        }
                    }
                }
            }

            // System.out.println( " xcos = " + xCos + " ycos = " + yCos + " zcos = " + zCos );
            if ( (xCos > yCos) && (xCos > zCos)) {
                orientation = FileInfoBase.SAGITTAL;
            } else if ( (yCos > xCos) && (yCos > zCos)) {
                orientation = FileInfoBase.CORONAL;
            } else if ( (zCos > xCos) && (zCos > yCos)) {
                orientation = FileInfoBase.AXIAL;
            } else if (!validInstanceSort) { // xLocation, yLocation, and zLocation were probably undefined and instance numbers equal.
                orientation = FileInfoBase.AXIAL;
                validOriSort = true;
                orient[0] = FileInfoBase.ORI_R2L_TYPE;
                orient[1] = FileInfoBase.ORI_A2P_TYPE;
                orient[2] = FileInfoBase.ORI_I2S_TYPE;
            }  else if ( (instanceNums.length > 1) && (instanceNums[0] != instanceNums[1])) { //fallback method to using instance number
                orientation = FileInfoBase.AXIAL;
                validInstanceSort = true;
                orient[0] = FileInfoBase.ORI_R2L_TYPE;
                orient[1] = FileInfoBase.ORI_A2P_TYPE;
                orient[2] = FileInfoBase.ORI_I2S_TYPE;
            } else {
                Preferences.debug("FileIO: Dicom orientation information could not be determined.  \n", Preferences.DEBUG_FILEIO);
            }
            
            if(validOriSort) {
                indices = zOri;
            } else if(validInstanceSort) {
                indices = zInst;
            } else if(validOri4DSort) { 
                indices = zOri4D;
            } else { //valid sorting could not be determined, order by file ordering
                for(int i=0; i<indices.length; i++) {
                    indices[i] = i;
                }
            }

            // problems if we reach this point!
            
        }

        // need to determine if it is ENHANCED DICOM and if its 4D
        final boolean isEnhanced = imageFile.isEnhanced();
        boolean isEnhanced4D = imageFile.isEnhanced4D();
        int enhancedNumSlices = 1;
        int enhancedNumVolumes = 1;
        if (isEnhanced) {
            enhancedNumSlices = imageFile.getEnhancedNumSlices();
        }

        if (nImages > 1) {
            int sliceDim = 0, timeDim = 0;
            extents = new int[4];
            if (isEnhanced4D) {
                extents[2] = enhancedNumSlices;
                enhancedNumVolumes = nImages / enhancedNumSlices;
                extents[3] = enhancedNumVolumes;
            } else if(validInstanceSort && refFileInfo.getTagTable().getValue("0020,0105") != null && refFileInfo.getTagTable().getValue("0020,1002") != null &&
                        (timeDim = Integer.valueOf(refFileInfo.getTagTable().getValue("0020,0105").toString()).intValue()) > 1 && 
                        (sliceDim = Integer.valueOf(refFileInfo.getTagTable().getValue("0020,1002").toString()).intValue()) > 1 &&
                        sliceDim*timeDim == nImages) {
                extents[3] = timeDim;
                extents[2] = sliceDim;
            } else if ((studyDescription != null && studyDescription.toUpperCase().contains("DTI")) && dtiSliceCounter != 0 || 
                            (seriesDescription != null && seriesDescription.toUpperCase().contains("DTI")) && dtiSliceCounter != 0) {
                if (scannerType != null && scannerType.toUpperCase().contains("PHILIPS")) { 
                    extents[2] = dtiSliceCounter;
                    extents[3] = nImages / dtiSliceCounter;
                }
                else if(scannerType != null && scannerType.toUpperCase().contains("GE")){
                    extents[2] = dtiSliceCounter;
                    extents[3] = nImages / dtiSliceCounter;
                }                  
                else{
                    extents = new int[3];
                    extents[2] = nImages;  
                }
            } 
            else{
                extents = new int[3];
                extents[2] = nImages;  
            }
        } else {
            extents = new int[2];

        }

        extents[0] = refFileInfo.getExtents()[0];
        extents[1] = refFileInfo.getExtents()[1];
        refFileInfo.setExtents(extents);

        ModelImage image = null;
        if (studyIDMaster.trim().equals("") && seriesNoRef.trim().equals("")) {
            image = new ModelImage(refFileInfo.displayType, extents, selectedFileName);
        } else {
            image = new ModelImage(refFileInfo.displayType, extents, studyIDMaster.trim() + "_" + seriesNoRef.trim());
        }
        int originalDataType = refFileInfo.displayType;
        boolean haveChangedType = false;

        if (refFileInfo.isMultiFrame() == true) {
            image.setFileInfo(refFileInfo, 0);
        }
        
        
        //Determine bvalue and gradient info for Siemen's DICOM
        
        

        // its probably a PET image an therefore reallocate data to store float image.
        if (refFileInfo.displayType != refFileInfo.getDataType()) {

            // TODO: the image was just created with refFileInfo.displayType... does this realloc have any effect
            //image.setType(refFileInfo.displayType);
            //image.reallocate(refFileInfo.displayType);
        }

        String filename;
        int start;
        int location;
        boolean multiframe = refFileInfo.isMultiFrame();

        // loop through files, place them in image array
        pBarVal = 0;

        // ENHANCED DICOM is multiframe...so we need to do matrix/orientation stuff here
        if (isEnhanced) {
            for(int i=0; i<image.getFileInfo().length; i++) {
                image.setFileInfo(refFileInfo, i);
            }
            matrix = refFileInfo.getPatientOrientation();

            if (matrix != null) {
                matrix.transform(refFileInfo.xLocation, refFileInfo.yLocation, refFileInfo.zLocation, tPt);
            }

            if (matrix != null) {

                // get back to original matrix
                float xCos = 0, yCos = 0, zCos = 0;
                xCos = Math.abs(matrix.get(2, 0));
                yCos = Math.abs(matrix.get(2, 1));
                zCos = Math.abs(matrix.get(2, 2));

                for (int j = 0; j < orient.length; j++) {
                    final int index = absBiggest(matrix.get(j, 0), matrix.get(j, 1), matrix.get(j, 2));

                    if (index == 0) {

                        if (matrix.get(j, 0) > 0) {
                            orient[j] = FileInfoBase.ORI_R2L_TYPE;
                        } else {
                            orient[j] = FileInfoBase.ORI_L2R_TYPE;
                        }
                    } else if (index == 1) {

                        if (matrix.get(j, 1) > 0) {
                            orient[j] = FileInfoBase.ORI_A2P_TYPE;
                        } else {
                            orient[j] = FileInfoBase.ORI_P2A_TYPE;
                        }
                    } else { // index == 2

                        if (matrix.get(j, 2) > 0) {
                            orient[j] = FileInfoBase.ORI_I2S_TYPE;
                        } else {
                            orient[j] = FileInfoBase.ORI_S2I_TYPE;
                        }
                    }
                }
                if ( (xCos > yCos) && (xCos > zCos)) {
                    orientation = FileInfoBase.SAGITTAL;
                } else if ( (yCos > xCos) && (yCos > zCos)) {
                    orientation = FileInfoBase.CORONAL;
                } else if ( (zCos > xCos) && (zCos > yCos)) {
                    orientation = FileInfoBase.AXIAL;
                }

            }
        }
        int enhancedCounter1 = 0;
        int enhancedCounter2 = 0;

        for (int i = 0; i < nImages; i++) {
            if (multiframe) {
                filename = fileList[0];
                start = i;
                location = i;
            } else {
                start = 0;
                location = i;
                if(performSort) {
                    filename = fileList[indices[i]];
                } else {
                    filename = fileList[i];
                }


                if (nImages == 1) {
                    location = 0;
                }
            }
            Preferences.debug("location: " + location + "\timg: " + filename + "\n", Preferences.DEBUG_FILEIO);

            try {

                if (i == (nImages - 1)) {
                    progressBar.updateValue(100, false);
                } else if ( ((float) i / (nImages - 1) * 100) > pBarVal) {
                    pBarVal += 10;
                    progressBar.updateValue(10 + Math.round((float) i / (nImages - 1) * 90), false);
                }

                // setting the refFileInfo doesn't matter here, since we will be overriding the file info shortly
                imageFile.setFileName(filename, refFileInfo);

                // Reuse header that was read in above
                FileInfoDicom curFileInfo;
                
                if ( !multiframe) {
                    if(performSort) {
                        curFileInfo = savedFileInfos[indices[i]];
                    } else {
                        curFileInfo = savedFileInfos[i];
                    }
                } else {
                    FileDicomTagTable table = refFileInfo.getTagTable();
                    refFileInfo.setTagTable(null);
                    curFileInfo = (FileInfoDicom) refFileInfo.clone();
                    refFileInfo.setTagTable(table);
                    curFileInfo.setTagTable(table);
                }
                
                if (location != 0 && isEnhanced && imageFile.getEnhancedTagTables() != null) {  //attach enhanced tag tables to image
                    imageFile.getEnhancedTagTables()[location - 1].isReferenceTagTable = false;
                    imageFile.getEnhancedTagTables()[location - 1].parentFileInfo = curFileInfo;
                    imageFile.getEnhancedTagTables()[location - 1].referenceTagTable = curFileInfo.getTagTable();
                    curFileInfo.setTagTable((FileDicomTagTable) imageFile.getEnhancedTagTables()[location - 1]);
                    curFileInfo.setInfoFromTags();
                }

                imageFile.setFileInfo(curFileInfo);

                // Read the image
                if (image.getType() == ModelStorageBase.FLOAT) {
                    imageFile.readImage(bufferFloat, curFileInfo.getDataType(), start);
                } else if (imageFile.isDir()) {
                    if (progressBar != null) {
                        progressBar.dispose();
                    }
                    return null;
                } else if (curFileInfo.getDataType() == ModelStorageBase.UINTEGER) {
                    imageFile.readImage(bufferInt, curFileInfo.getDataType(), start);
                } else {
                    imageFile.readImage(bufferShort, curFileInfo.getDataType(), start);
                    if ((!haveChangedType) && (refFileInfo.getDataType() != originalDataType)) {
                        haveChangedType = true;
                        image.setType(refFileInfo.displayType);
                        image.reallocate(refFileInfo.displayType);
                    }
                }
                if ( !isEnhanced4D) {
                    curFileInfo.setExtents(extents);
                } else {
                    curFileInfo.setExtents(extents);
                    final float[] res = curFileInfo.getResolutions();
                    final float[] newRes = {res[0], res[1], res[2], 1};
                    curFileInfo.setResolutions(newRes);
                }

                curFileInfo.setAxisOrientation(orient);
                curFileInfo.setImageOrientation(orientation);

                final float[] origin = new float[3];
                final float[] newOriginPt = new float[3];

                origin[0] = curFileInfo.xLocation;
                origin[1] = curFileInfo.yLocation;
                origin[2] = curFileInfo.zLocation;
                
                // TransMatrix dicomMatrix = (TransMatrix) (getMatrix().clone());
                // Finally convert the point to axial millimeter DICOM space.
                // dicomMatrix.transform(coord, tCoord);

                for (int j = 0; j < 3; j++) {

                    if ( (orient[j] == FileInfoBase.ORI_L2R_TYPE) || (orient[j] == FileInfoBase.ORI_R2L_TYPE)) {
                        newOriginPt[j] = origin[0];
                    } else if ( (orient[j] == FileInfoBase.ORI_P2A_TYPE) || (orient[j] == FileInfoBase.ORI_A2P_TYPE)) {
                        newOriginPt[j] = origin[1];
                    } else if ( (orient[j] == FileInfoBase.ORI_S2I_TYPE) || (orient[j] == FileInfoBase.ORI_I2S_TYPE)) {
                        newOriginPt[j] = origin[2];
                    }
                }

                if ( !isEnhanced4D) {
                    if(extents.length > newOriginPt.length) {
                        float[] newOriginPt4D = new float[extents.length];
                        int j=0;
                        for(j=0; j<newOriginPt.length; j++) {
                            newOriginPt4D[j] = newOriginPt[j];
                        }
                        
                        for(j=j; j<newOriginPt4D.length; j++) {
                            newOriginPt4D[j] = 0;
                        }
                        
                        curFileInfo.setOrigin(newOriginPt4D);
                    } else {    
                        curFileInfo.setOrigin(newOriginPt);
                    }
                } else {
                    final float[] newOriginPt4D = new float[4];
                    newOriginPt4D[0] = newOriginPt[0];
                    newOriginPt4D[1] = newOriginPt[1];
                    newOriginPt4D[2] = newOriginPt[2];
                    newOriginPt4D[3] = 0;
                    curFileInfo.setOrigin(newOriginPt4D);
                }
                
                image.setFileInfo(curFileInfo, location);
                
                if (isEnhanced4D) {
                    if (enhancedCounter1 == enhancedNumVolumes) {
                        enhancedCounter1 = 0;
                        enhancedCounter2++;
                    }
                    location = (enhancedCounter1 * enhancedNumSlices) + enhancedCounter2;
                    enhancedCounter1++;
                }
                
                if (image.getType() == ModelStorageBase.FLOAT) {
                    image.importData(location * length, bufferFloat, false);
                } else if (image.getType() == ModelStorageBase.UINTEGER) {
                    image.importData(location * length, bufferInt, false);
                } else {
                    image.importData(location * length, bufferShort, false);
                }
            } catch (final IOException error) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                    Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
                } else {
                    Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
                }

                error.printStackTrace();

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (final OutOfMemoryError error) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                    Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
                } else {
                    Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
                }

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                error.printStackTrace();

                System.gc();

                return null;
            }
        }

        // Save the DICOM tag 0020,0037 Image Orientation to the image transformation matrix.
        if (matrix != null) {
            final TransMatrix invMatrix = ( (matrix.clone()));

            try {
                invMatrix.Inverse();
                invMatrix.setTransformID(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL);

                image.getMatrixHolder().addMatrix(invMatrix);

                // image.setMatrix(invMatrix);

                // for (int m = 0; m < nImages; m++) {
                // image.getFileInfo(m).setTransformID(FileInfoBase.TRANSFORM_SCANNER_ANATOMICAL);
                // }
            } catch (final RuntimeException rte) {
                invMatrix.MakeIdentity();
                // MipavUtil.displayError("Error = " + rte);
            }
        }

        if (nListImages > 1) {

            for (int m = 0; m < nImages; m++) {
                image.getFileInfo(m).setMultiFile(true);
            }
        }

        final FileDicomTagTable firstSliceTagTable = ((FileInfoDicom) image.getFileInfo(0)).getTagTable();

        if (nImages > 1) {

            float sliceThickness = -1;
            float sliceSpacing = -1;

            boolean getValidZRes = false;
            // First check slice spacing tag for getting validzRes:
            if ( (firstSliceTagTable.get("0018,0050") != null) || (firstSliceTagTable.get("0018,0088") != null)) {

                if ((String) firstSliceTagTable.getValue("0018,0050") != null) {
                    try {
                        sliceThickness = Float.parseFloat((String) firstSliceTagTable.getValue("0018,0050"));
                    } catch (final NumberFormatException nfe) {
                        Preferences.debug("0018,0050:\tInvalid float value found in slice thickness tag.",
                                Preferences.DEBUG_FILEIO);
                    }
                }

                // 0018,0088 = Spacing Between Slices
                // 0018,0050 = Slice Thickness
                if ((String) firstSliceTagTable.getValue("0018,0088") != null) {
                    try {
                        sliceSpacing = Float.parseFloat((String) firstSliceTagTable.getValue("0018,0088"));
                        for (int m = 0; m < nImages; m++) {
                            image.getFileInfo(m).setResolutions(sliceSpacing, 2);
                        }
                        getValidZRes = true;
                    } catch (final NumberFormatException nfe) {
                        Preferences.debug("0018,0088:\tInvalid float value found in slice spacing tag.",
                                Preferences.DEBUG_FILEIO);
                    }
                }

                // System.err.println("Slice Spacing: " + sliceSpacing);
                if (sliceThickness == -1 && sliceSpacing != -1) {
                    sliceThickness = sliceSpacing;
                    // System.err.println("Slice Thickness: " + sliceThickness);
                }

                if (sliceThickness > 0) {
                    for (int m = 0; m < nImages; m++) {
                        image.getFileInfo(m).setSliceThickness(sliceThickness);
                    }
                }
            }

            float sliceDifference = -1;

            // if slice spacing tag wasn't there or was 0, check slice location (and take the difference)
            if (!getValidZRes && (firstSliceTagTable.get("0020,1041") != null) && (sliceThickness == -1)) {

                if ((String) firstSliceTagTable.getValue("0020,1041") != null) {
                    sliceDifference = Float.parseFloat((String) ((FileInfoDicom) image.getFileInfo(1)).getTagTable()
                            .getValue("0020,1041"))
                            - Float.parseFloat((String) firstSliceTagTable.getValue("0020,1041"));

                    // System.err.println("Slice difference: " + sliceDifference);
                    if ( (Math.abs(sliceDifference) < sliceThickness) && (Math.abs(sliceDifference) > 0.001)) {
                        image.getFileInfo(0).setResolutions(sliceDifference, 2);

                        for (int m = 1; m < (nImages - 1); m++) {
                            image.getFileInfo(m).setResolutions(
                                    Float.parseFloat((String) ((FileInfoDicom) image.getFileInfo(m + 1)).getTagTable()
                                            .getValue("0020,1041"))
                                            - Float.parseFloat((String) ((FileInfoDicom) image.getFileInfo(m))
                                                    .getTagTable().getValue("0020,1041")), 2);
                        }

                        if (nImages > 2) {
                            image.getFileInfo(nImages - 1).setResolutions(
                                    image.getFileInfo(nImages - 2).getResolution(2), 2);
                        }
                    }
                    
                    getValidZRes = true;
                }
            }
            
            // finally, if slice spacing failed to produce a z-res,
            // check for image position
            if (!getValidZRes && firstSliceTagTable.get("0020,0032") != null) {
                double xLoc0 = ((FileInfoDicom) image.getFileInfo(0)).xLocation;
                double yLoc0 = ((FileInfoDicom) image.getFileInfo(0)).yLocation;
                double zLoc0 = ((FileInfoDicom) image.getFileInfo(0)).zLocation;

                double xLoc1 = ((FileInfoDicom) image.getFileInfo(1)).xLocation;
                double yLoc1 = ((FileInfoDicom) image.getFileInfo(1)).yLocation;
                double zLoc1 = ((FileInfoDicom) image.getFileInfo(1)).zLocation;

                double res3Dim = 1;

                res3Dim = ( (xLoc0 - xLoc1) * (xLoc0 - xLoc1)) + ( (yLoc0 - yLoc1) * (yLoc0 - yLoc1))
                        + ( (zLoc0 - zLoc1) * (zLoc0 - zLoc1));
                res3Dim = Math.sqrt(res3Dim);

                // System.err.println("res3Dim Spacing: " + res3Dim);
                if ( (res3Dim != 0)) {
                    image.getFileInfo(0).setResolutions((float) res3Dim, 2);

                    // System.out.println (" res3Dim 1 = " + res3Dim);
                    for (int m = 1; m < (nImages - 1); m++) {

                        xLoc0 = ((FileInfoDicom) image.getFileInfo(m)).xLocation;
                        yLoc0 = ((FileInfoDicom) image.getFileInfo(m)).yLocation;
                        zLoc0 = ((FileInfoDicom) image.getFileInfo(m)).zLocation;

                        xLoc1 = ((FileInfoDicom) image.getFileInfo(m + 1)).xLocation;
                        yLoc1 = ((FileInfoDicom) image.getFileInfo(m + 1)).yLocation;
                        zLoc1 = ((FileInfoDicom) image.getFileInfo(m + 1)).zLocation;

                        res3Dim = ( (xLoc0 - xLoc1) * (xLoc0 - xLoc1)) + ( (yLoc0 - yLoc1) * (yLoc0 - yLoc1))
                                + ( (zLoc0 - zLoc1) * (zLoc0 - zLoc1));
                        res3Dim = Math.sqrt(res3Dim);

                        image.getFileInfo(m).setResolutions((float) res3Dim, 2);
                    }

                    image.getFileInfo(nImages - 1).setResolutions((float) res3Dim, 2);
                    getValidZRes = true;
                }
            }
            
            //if calculations were not successful and sliceThickness tag exists, populate with slice thickness
            if(!getValidZRes && sliceThickness != -1) {
                for (int m = 0; m < nImages; m++) {
                    image.getFileInfo(m).setResolutions(sliceThickness, 2);
                }
                getValidZRes = true;
            }
            
            // see if we found z-res somewhere
            if (!getValidZRes) {
                Preferences.debug("error calculating z-resolution in FileIO.readDicom()", Preferences.DEBUG_FILEIO);
            }
        }

        // That the image contains this tag, means that the image contains it's own
        // LUT, and that we should use it.
        if (firstSliceTagTable.get("0028,1201") != null) {
            LUT = ((FileInfoDicom)image.getFileInfo(0)).getLUT();
        }

        if (progressBar != null) {
            progressBar.dispose();
        }
        imageFile.finalize();
        imageFile = null;
        matrix = null;
        // refFileInfo = null;
        bufferShort = null;
        bufferFloat = null;
        
        //Determines if image is DWI then saves parameters to DTIParameters object
        FileInfoDicom dicomInfo = (FileInfoDicom) image.getFileInfo(0);
        FileDicomTagTable tagTable = dicomInfo.getTagTable();
        studyDescription = (String) tagTable.getValue("0008,1030");
        seriesDescription = (String) tagTable.getValue("0008,103E");
        scannerType = (String) tagTable.getValue("0008,0070");

        if ((studyDescription != null && studyDescription.toUpperCase().contains("DTI")) || 
                (seriesDescription != null && seriesDescription.toUpperCase().contains("DTI"))) {
           if (scannerType != null && scannerType.toUpperCase().contains("SIEMEN")) {
                //For Siemen's DWI 3D Mosaic Images
                if (image.is3DImage()) {
                    int numVolumes = image.getExtents()[2];
                    dtiparams = new DTIParameters(numVolumes);
                    dtiparams.setNumVolumes(numVolumes);
                    
                    if ((String) tagTable.getValue("0019,100C") != null){
                        float [] flBvalueArray = new float[numVolumes];
                        float[][] flGradientArray = new float[numVolumes][3];
                        float[][] flBmatrixArray = new float[numVolumes][6];
                        
                        for (int i = 0; i < numVolumes; i++) {                   
                            // Get Bvalues from Siemens Tags
                            FileInfoDicom dicom3dInfo = (FileInfoDicom) image.getFileInfo(i);
                            FileDicomTagTable tag3dTable = dicom3dInfo.getTagTable();
                            String siemenBvalue = (String) tag3dTable.getValue("0019,100C");
                            flBvalueArray[i] = Float.parseFloat(siemenBvalue);
                            
                            if ((String) tagTable.getValue("0019,1027") != null){
                                String siemenBmatrix = (String) dicom3dInfo.getTagTable().getValue("0019,1027");
                                siemenBmatrix = siemenBmatrix.trim();
                                String bMatrix = siemenBmatrix.replace('\\', '\t');
                                final String[] arr2 = bMatrix.split("\t");
                                flBmatrixArray[i][0] = Float.valueOf(arr2[0]);
                                flBmatrixArray[i][1] = Float.valueOf(arr2[1]);
                                flBmatrixArray[i][2] = Float.valueOf(arr2[2]);
                                flBmatrixArray[i][3] = Float.valueOf(arr2[3]);
                                flBmatrixArray[i][4] = Float.valueOf(arr2[4]);
                                flBmatrixArray[i][5] = Float.valueOf(arr2[5]);
                            }
    
                            // Get Gradients from Siemens Tags
                            /*if ((String) tagTable.getValue("0019,100E") != null){
                                String siemenGrads = (String) tag3dTable.getValue("0019,100E");
                                siemenGrads = siemenGrads.trim();
                                String grads = siemenGrads.replace('\\', '\t');
                                final String[] arr2 = grads.split("\t");
                                flGradientArray[i][0] = Float.valueOf(arr2[0]);
                                flGradientArray[i][1] = Float.valueOf(arr2[1]);
                                flGradientArray[i][2] = Float.valueOf(arr2[2]);
                            }*/
                        }
                        dtiparams.setbValues(flBvalueArray);
                        dtiparams.setbMatrixVals(flBmatrixArray);
                        //dtiparams.setGradients(flGradientArray);
                    }

                } else if (image.is4DImage()) {
                    //For Siemen's DWI 4D Volume Images
                     int numVolumes = image.getExtents()[3];
                     dtiparams = new DTIParameters(numVolumes);
                     dtiparams.setNumVolumes(numVolumes);
                     
                     if ((String) tagTable.getValue("0019,100C") != null){
                         float [] flBvalueArray = new float[numVolumes];
                         float[][] flGradientArray = new float[numVolumes][3];
                         float[][] flBmatrixArray = new float[numVolumes][6];
    
                         for (int i = 0; i < numVolumes; i++) {
                            FileInfoDicom dicom4dInfo = (FileInfoDicom) image.getFileInfo(i * image.getExtents()[2]);
                            
                            // Get Bvalues from Siemens Tags
                            FileDicomTagTable tag4dTable = dicom4dInfo.getTagTable();
                            String siemenBvalue = (String) tag4dTable.getValue("0019,100C");
                            flBvalueArray[i] = Float.parseFloat(siemenBvalue);
                            
                            //Get B-matrix from Siemen's Tags
                            if ((String) tagTable.getValue("0019,1027") != null){
                            String siemenBmatrix = (String) tag4dTable.getValue("0019,1027");
                                siemenBmatrix = siemenBmatrix.trim();
                                String bMatrix = siemenBmatrix.replace('\\', '\t');
                                final String[] arr2 = bMatrix.split("\t");
                                flBmatrixArray[i][0] = Float.valueOf(arr2[0]);
                                flBmatrixArray[i][1] = Float.valueOf(arr2[1]);
                                flBmatrixArray[i][2] = Float.valueOf(arr2[2]);
                                flBmatrixArray[i][3] = Float.valueOf(arr2[3]);
                                flBmatrixArray[i][4] = Float.valueOf(arr2[4]);
                                flBmatrixArray[i][5] = Float.valueOf(arr2[5]);
                        }
    
                            // Get Gradients from Siemens Tags
                            /*if ((String) tagTable.getValue("0019,100E") != null){
                                String siemenGrads = (String) tag4dTable.getValue("0019,100E");
                                siemenGrads = siemenGrads.trim();
                                String grads = siemenGrads.replace('\\', '\t');
                                final String[] arr2 = grads.split("\t");
                                flGradientArray[i][0] = Float.valueOf(arr2[0]);
                                flGradientArray[i][1] = Float.valueOf(arr2[1]);
                                flGradientArray[i][2] = Float.valueOf(arr2[2]);
                            }*/
                        }
                        
                    dtiparams.setbValues(flBvalueArray);
                    dtiparams.setbMatrixVals(flBmatrixArray);
                    //dtiparams.setGradients(flGradientArray);

                }
              }
            }
           else if (scannerType != null && scannerType.toUpperCase().contains("PHILIPS")) {                       
                    if (image.is4DImage()){
                        //For Philip's DWI 4D DICOM
                        int numVolumes = image.getExtents()[3];                    
                        dtiparams = new DTIParameters(numVolumes);
                        dtiparams.setNumVolumes(numVolumes);
                    
                        if ((String) tagTable.getValue("0018,9087") != null){
                            float [] flBvalueArray = new float[numVolumes];
                            float[][] flGradientArray = new float[numVolumes][3];
       
                            for (int i = 0; i < numVolumes; i++) {
                               FileInfoDicom dicom4dInfo = (FileInfoDicom) image.getFileInfo(i*image.getExtents()[2]);
                               
                               // Get Bvalues from Philips Tags
                               FileDicomTagTable tag4dTable = dicom4dInfo.getTagTable();
                               String philipsBvalue = (String) tag4dTable.getValue("0018,9087");
                               flBvalueArray[i] = Float.parseFloat(philipsBvalue);
       
                               // Get Gradients from Philips Tags
                               if ((String) tagTable.getValue("0018,9089") != null){
                               String philipsGrads = (String) tag4dTable.getValue("0018,9089");
                               philipsGrads = philipsGrads.trim();
                               String grads = philipsGrads.replace('\\', '\t');
                               final String[] arr2 = grads.split("\t");
                               flGradientArray[i][0] = Float.valueOf(arr2[1]);
                               flGradientArray[i][1] = Float.valueOf(arr2[2]);
                               flGradientArray[i][2] = Float.valueOf(arr2[0]);
                               }
                           }                          
                           dtiparams.setbValues(flBvalueArray);
                           dtiparams.setGradients(flGradientArray);   
                           }
                    }
                                
            
        }// if Philips
          else if (scannerType != null && scannerType.toUpperCase().contains("GE")) {  
              if (image.is4DImage()){
                  //For GE DWI 4D DICOM
                  int numVolumes = image.getExtents()[3];             
                  dtiparams = new DTIParameters(numVolumes);
                  dtiparams.setNumVolumes(numVolumes);
              
                  if ((String) tagTable.getValue("0019,10BB") != null){
                      float [] flBvalueArray = new float[numVolumes];
                      float[][] flGradientArray = new float[numVolumes][3];
 
                      for (int i = 0; i < numVolumes; i++) {
                         FileInfoDicom dicom4dInfo = (FileInfoDicom) image.getFileInfo(i*image.getExtents()[2]);
                         
                         // Get Bvalues from Philips Tags
                         FileDicomTagTable tag4dTable = dicom4dInfo.getTagTable();
                         String geBvalue = (String) tag4dTable.getValue("0043,1039");
                         geBvalue = geBvalue.trim();
                         String bval = geBvalue.replace('\\', '\t');
                         final String[] arr2 = bval.split("\t");
                         flBvalueArray[i] = Float.valueOf(arr2[0]);
 
                         // Get Gradients from Philips Tags
                         if ((String) tagTable.getValue("0019,10BB") != null){
                         flGradientArray[i][0] = Float.valueOf((String) tag4dTable.getValue("0019,10BB"));
                         flGradientArray[i][1] = Float.valueOf((String) tag4dTable.getValue("0019,10BC"));
                         flGradientArray[i][2] = Float.valueOf((String) tag4dTable.getValue("0019,10BD"));
                         }
                     }                    
                     dtiparams.setbValues(flBvalueArray);
                     dtiparams.setGradients(flGradientArray);
                     }
              }
               
           }//if GE
      }
        
        if (dtiparams != null){
            image.setDTIParameters(dtiparams);
        }
        
        return image;
    }

    private int sortDtiDicomData(String studyDescription, String seriesDescription, String scannerType, FileDicomTagTable tagTable, FileInfoDicom[] savedFileInfos, int[] indices, int instanceNumsLength, int[] zInst) {
      //Sorts DWI images based on gradient values corresponding to each volume in the series
        int dtiSliceCounter = -1;
        if (scannerType != null && scannerType.toUpperCase().contains("PHILIPS")) { 
            if ((String) tagTable.getValue("0018,9089") != null){
                dtiSliceCounter = 0;
                int dtiSliceCounter2 = 0;// Helps check for incomplete DWI series
                ArrayList<Integer> IndexVolArrayList = new ArrayList<Integer>() ;
                
                for (int i = 0; i < instanceNumsLength; i++) {
                    String savedFileGradFirst = (String) savedFileInfos[0].getTagTable().getValue("0018,9089");
                    String savedFileGradSecond = (String) savedFileInfos[1].getTagTable().getValue("0018,9089");
                    String savedFileGradI = (String)savedFileInfos[i].getTagTable().getValue("0018,9089");
                    String savedFilebvalFirst = (String) savedFileInfos[0].getTagTable().getValue("0018,9087");
                    String savedFilebvalSecond = (String) savedFileInfos[1].getTagTable().getValue("0018,9087");
                    String savedFilebvalI = (String)savedFileInfos[i].getTagTable().getValue("0018,9087");
                    if (savedFileGradFirst.equals(savedFileGradI) && savedFilebvalFirst.equals(savedFilebvalI) ){
                        //Stores order of gradients
                        IndexVolArrayList.add(dtiSliceCounter,i);
                        dtiSliceCounter++;
                    }
                    else if(savedFileGradSecond.equals(savedFileGradI) && savedFilebvalSecond.equals(savedFilebvalI)){
                        dtiSliceCounter2++;
                    }
                }

                //Checks for incomplete DWI series                                  
                if (dtiSliceCounter == 1 || dtiSliceCounter == 0 || dtiSliceCounter != dtiSliceCounter2++){
                    dtiSliceCounter = -1; //Not complete DWI series
                    }
                    
                else{
                    int rintCounter = 0;
                    int tVolumeNum = (IndexVolArrayList.get(1)-IndexVolArrayList.get(0));
                       for (int i = 0; i <tVolumeNum; i++) {
                           for (int j=0; j< IndexVolArrayList.size(); j++){
                               //rint populated by ordering of instance nums based on volume order
                               zInst[rintCounter] = IndexVolArrayList.get(j) + i;
                               rintCounter++;                                            
                           }
                       }                                
                }
             }
           }// if PHILIPS Note: New sort method may need to be 
              //added for different versions of Philips DWI dicom data sets. See GE code
        else if (scannerType != null && scannerType.toUpperCase().contains("GE")) {  
            if ((String) tagTable.getValue("0019,10BB") != null && (String) tagTable.getValue("0019,10BC") != null
                    && (String) tagTable.getValue("0019,10BD") != null && (String) tagTable.getValue("0043,1039") != null){
                dtiSliceCounter = 0;
                int dtiSliceCounter2 = 0;// Checks for incomplete DWI series and determines order of grads
                int dtiSliceCounter3 = 0;// Checks for incomplete DWI series and determines order of grads 
                ArrayList<Integer> IndexVolArrayList = new ArrayList<Integer>() ;
                for (int i = 0; i < instanceNumsLength; i++) {
                    String savedFileGradFirst = (String)savedFileInfos[0].getTagTable().getValue("0019,10BB")+
                            (String)savedFileInfos[0].getTagTable().getValue("0019,10BC")+
                            (String)savedFileInfos[0].getTagTable().getValue("0019,10BD") ;
                    String savedFileGradSecond = (String)savedFileInfos[1].getTagTable().getValue("0019,10BB")+
                    (String)savedFileInfos[1].getTagTable().getValue("0019,10BC")+
                    (String)savedFileInfos[1].getTagTable().getValue("0019,10BD") ;
                    String savedFileGradI = (String)savedFileInfos[i].getTagTable().getValue("0019,10BB")+
                    (String)savedFileInfos[i].getTagTable().getValue("0019,10BC")+
                    (String)savedFileInfos[i].getTagTable().getValue("0019,10BD") ;
                    String savedFilebvalFirst = (String) savedFileInfos[0].getTagTable().getValue("0043,1039");
                    String savedFilebvalSecond = (String) savedFileInfos[1].getTagTable().getValue("0043,1039");
                    String savedFilebvalI = (String)savedFileInfos[i].getTagTable().getValue("0043,1039");
                    if (savedFileGradFirst.equals(savedFileGradI) && savedFilebvalFirst.equals(savedFilebvalI) ){
                        //Stores order of gradients
                        IndexVolArrayList.add(dtiSliceCounter,i);
                        dtiSliceCounter++;
                    }
                    else if(savedFileGradSecond.equals(savedFileGradI) && savedFilebvalSecond.equals(savedFilebvalI)){
                        dtiSliceCounter2++;
                    }
                }
                if (dtiSliceCounter != 1 && dtiSliceCounter != 0 &&
                        (String)savedFileInfos[dtiSliceCounter].getTagTable().getValue("0019,10BB")!= null && 
                        (String)savedFileInfos[dtiSliceCounter+1].getTagTable().getValue("0019,10BB")!= null){
                    if( dtiSliceCounter != dtiSliceCounter2){
                        for (int i = 0; i < dtiSliceCounter; i++){
                             String savedFileGrad1SecondVol = (String)savedFileInfos[dtiSliceCounter].getTagTable().getValue("0019,10BB")+
                            (String)savedFileInfos[dtiSliceCounter].getTagTable().getValue("0019,10BC")+
                            (String)savedFileInfos[dtiSliceCounter].getTagTable().getValue("0019,10BD");
                             String savedFileGradISecondVol = (String)savedFileInfos[dtiSliceCounter+i].getTagTable().getValue("0019,10BB")+
                             (String)savedFileInfos[dtiSliceCounter+i].getTagTable().getValue("0019,10BC")+
                             (String)savedFileInfos[dtiSliceCounter+i].getTagTable().getValue("0019,10BD");
                             String savedFilebvalSecond = (String) savedFileInfos[1].getTagTable().getValue("0043,1039");
                             String savedFilebvalI = (String)savedFileInfos[i].getTagTable().getValue("0043,1039");
                             if(savedFileGrad1SecondVol.equals(savedFileGradISecondVol) && savedFilebvalSecond.equals(savedFilebvalI)){
                                 dtiSliceCounter3++;   
                             }
                        }
                        if (dtiSliceCounter == dtiSliceCounter3){
                            isDTISort = true;
                            int rintCounter = 0;
                            int tVolumeNum = instanceNumsLength/dtiSliceCounter;
                            float [][] instanceNumIndices = new float[tVolumeNum][IndexVolArrayList.size()];
                               for (int i = 0; i <tVolumeNum; i++) {
                                   for (int j=0; j< IndexVolArrayList.size(); j++){
                                       //rint populated by ordering of instance nums based on volume order
                                       zInst[rintCounter] = IndexVolArrayList.get(j) + (i*IndexVolArrayList.size());
                                       rintCounter++;                                            
                                   }
                               }                              
                        }
                        else{
                            dtiSliceCounter = -1; //Not complete DWI series
                        }
                    }

                    else if (dtiSliceCounter == dtiSliceCounter2){
                        isDTISort = true;
                        int rintCounter = 0;
                        int tVolumeNum = (IndexVolArrayList.get(1)-IndexVolArrayList.get(0));
                           for (int i = 0; i <tVolumeNum; i++) {
                               for (int j=0; j< IndexVolArrayList.size(); j++){
                                   //rint populated by ordering of instance nums based on volume order
                                   zInst[rintCounter] = IndexVolArrayList.get(j) + i;
                                   rintCounter++;                                            
                               }
                           }                                  
                    } else {
                        dtiSliceCounter = -1; //Not complete DWI series 
                    }

                } else {
                    dtiSliceCounter = -1; //Not complete DWI series 
                }

             }  
        } else{ //For DTI dicom data not aquired from GE or Philips scanners
            dtiSliceCounter = -1;
        }
        
        if(dtiSliceCounter != -1) {
            for(int i=0; i<indices.length; i++) {
                indices[i] = zInst[i];
            }
        }
        
        return dtiSliceCounter;
        
    }

    /**
     * partition into image subsets using media instance uids
     */
    private int[] tryPartitionByMediaUID(FileInfoDicom[] savedFileInfos) {
        try {
            
            HashMap<Integer, ArrayList<Object[]>> v = new HashMap<Integer, ArrayList<Object[]>>();
            String[] mediaStringAr;
            Integer mediaInt;
            for(int i=0; i<savedFileInfos.length; i++) {
                mediaStringAr = savedFileInfos[i].getTagTable().getValue("0002,0003").toString().split("\\.");
                mediaInt = Integer.valueOf(mediaStringAr[mediaStringAr.length-2]);
                if(!v.containsKey(mediaInt)) {
                    v.put(mediaInt, new ArrayList<Object[]>());
                } 
                
                v.get(mediaInt).add(new Object[]{savedFileInfos[i], i});
            }
            
            //sort each sub-collection by instance number
            ArrayList<Object[]> ar = new ArrayList<Object[]>();
            ArrayList<Integer> keySet;
            Collections.sort(keySet = new ArrayList<Integer>(v.keySet()));
            if(keySet.size() <= 1) {
                return null; //only done when multiple SOP
            }
            for(Integer i : keySet) {   
                Collections.sort(v.get(i), new Comparator<Object[]>(){
                    
                    public int compare(Object[] arg0, Object[] arg1) {
                        return ((FileInfoDicom)arg0[0]).instanceNumber - ((FileInfoDicom)arg1[0]).instanceNumber;
                    }   
                });
                ar.addAll(v.get(i));
            }
            
            int[] indices = new int[savedFileInfos.length];
            for(int i=0; i<indices.length; i++) {
                indices[i] = (Integer) ar.get(i)[1];
            }
            return indices;
        } catch(Exception e) {
            Preferences.debug("Failed to partition based on media instance uids");
        }
        
        return null;
    }

    private String[] removeFromImageList(String selectedFileName, String[] fileList) {
        final String[] fileListTemp = new String[fileList.length - 1];

        for (int i = 0, j=0; i < fileListTemp.length; i++, j++) {
            if (fileList[i].contains(selectedFileName)) {
                j++;
            }
            fileListTemp[i] = fileList[j]; 
        }
        fileList = fileListTemp;

        return fileList;
    }
    
    /**
     * Gets the value of the Dicom modality tag, 0008,0060. SR tags indicate a structured report file.
     * 
     * @param imageFile
     * @return
     */
    private String getModality(final FileDicom imageFile) {
        final FileDicomTag modalityTag = ((FileInfoDicom) imageFile.getFileInfo()).getTagTable().get(
                new FileDicomKey("0008,0060"));
        if (modalityTag != null && modalityTag.getValue(true) != null) {
            return modalityTag.getValue(true).toString();
        }
        return null;
    }

    /**
     * Reads generic file from an absolute filename.
     * 
     * @param absoluteFilename String - the absolute filename, including the path
     * 
     * @return ModelImage
     */
    public ModelImage readImage(final String absoluteFilename) {

        if (absoluteFilename == null) {
            return null;
        }

        final int lastIndex = absoluteFilename.lastIndexOf(File.separatorChar);

        if (lastIndex == -1) {

            // failed
            return null;
        }

        final String path = absoluteFilename.substring(0, lastIndex);
        final String filename = absoluteFilename.substring(lastIndex);

        return readImage(filename, path);
    }

    /**
     * Reads file, determines file type, and calls a read function specific to the file. That read function returns an
     * image and this function stores it in ModelImage. File is not multi file, file info is not previously known,
     * there's no "second address" for AFNI, and this is not an image B.
     * 
     * @param fileName File name where image is located.
     * @param fileDir File directory where image is located.
     * 
     * @return The image that was read in from the file.
     */
    public ModelImage readImage(final String fileName, final String fileDir) {
        return readImage(fileName, fileDir, false, null, 0, false, false);
    }

    /**
     * Reads file, determines file type, and calls a read function specific to the file. That read function returns an
     * image and this function stores it in ModelImage.
     * 
     * @param fileName File name where image is located.
     * @param fileDir File directory where image is located.
     * @param multiFile Flag indicating multi file.
     * @param fileInfo File info already known; will usually be null, but valid if called from script parser.
     * 
     * @return The image that was read in from the file.
     */
    public ModelImage readImage(final String fileName, final String fileDir, final boolean multiFile,
            final FileInfoBase fileInfo) {
        return readImage(fileName, fileDir, multiFile, fileInfo, 0, false, false);
    }

    /**
     * Reads file, determines file type, and calls a read function specific to the file. That read function returns an
     * image and this function stores it in ModelImage.
     * 
     * @param fileName File name where image is located.
     * @param fileDir File directory where image is located.
     * @param multiFile Flag indicating multi file.
     * @param fileInfo File info already known; will usually be null, but valid if called from script parser.
     * @param loadB Flag indicating if this is an image B.
     * 
     * @return The image that was read in from the file.
     */
    public ModelImage readImage(final String fileName, final String fileDir, final boolean multiFile,
            final FileInfoBase fileInfo, final boolean loadB) {
        return readImage(fileName, fileDir, multiFile, fileInfo, 0, loadB, false);
    }

    /**
     * Reads file, determines file type, and calls a read function specific to the file. That read function returns an
     * image and this function stores it in ModelImage.
     * 
     * @param fileName File name where image is located.
     * @param fileDir File directory where image is located.
     * @param multiFile Flag indicating multi file.
     * @param fileInfo File info already known; will usually be null, but valid if called from script parser.
     * @param secondAddress Address of second TIFF header
     * 
     * @return The image that was read in from the file.
     */
    public ModelImage readImage(final String fileName, final String fileDir, final boolean multiFile,
            final FileInfoBase fileInfo, final int secondAddress) {
        return readImage(fileName, fileDir, multiFile, fileInfo, secondAddress, false, false);
    }

    /**
     * Reads file, determines file type, and calls a read function specific to the file. That read function returns an
     * image and this function stores it in ModelImage.
     * 
     * @param fileName File name where image is located.
     * @param fileDir File directory where image is located.
     * @param multiFile Flag indicating multi file loading of images from directory. <code>true</code> is load images
     *            of this filetype to be loaded as a set (when this feature is supported).
     * @param fileInfo File info already known; will usually be null, but valid if called from script parser.
     * @param secondAddress Address of second TIFF header
     * @param loadB Flag indicating if this is an image B.
     * @param one A load-single flag. <code>true</code> indicates that the method is to load only the file that is
     *            defined by the <code>fileName</code>, rather than a multiple file load of a 3d Image.
     * 
     * @return The image that was read in from the file.
     */
    public ModelImage readImage(String fileName, String fileDir, boolean multiFile, final FileInfoBase fileInfo,
            final int secondAddress, final boolean loadB, final boolean one) {
        int index;

        File file;
        FileInputStream fis;
        ZipInputStream zin;
        GZIPInputStream gzin;
        CBZip2InputStream bz2in;
        FileOutputStream out;
        int bytesRead;
        ModelImage image = null;
        int fileType = FileUtility.UNDEFINED;
        int userDefinedFileType = 0;
        String userDefinedSuffix = null;
        String uncompressedName = null;
        String compressedName = null;
        String compressedDir = null;
        String tempDir = null;
        if ( (fileName == null) || (fileDir == null)) {
            return null;
        }
        boolean unzip;
        boolean gunzip;
        boolean bz2unzip;

        this.fileDir = fileDir;
        fileName = fileName.trim();

        index = fileName.lastIndexOf(".");

        if (fileName.substring(index + 1).equalsIgnoreCase("zip")) {
            unzip = true;
        } else {
            unzip = false;
        }

        if (fileName.substring(index + 1).equalsIgnoreCase("gz")) {
            gunzip = true;
        } else {
            gunzip = false;
        }

        if (fileName.substring(index + 1).equalsIgnoreCase("bz2")) {
            bz2unzip = true;
        } else {
            bz2unzip = false;
        }
        boolean niftiCompressed = false;
        if (unzip || gunzip || bz2unzip) {
            // if NIFTI....dont unzip/gunzip/bz2 and write out to temp dir here..we will directly stream in FileNIFTI
            final String sub = fileName.substring(0, index);
            if (sub.substring(sub.lastIndexOf(".") + 1, sub.length()).equalsIgnoreCase("nii")) {
                niftiCompressed = true;
            } else {
                tempDir = Preferences.getFileTempDir();
                if (tempDir == null) {
                    tempDir = System.getProperty("user.home") + File.separator + "mipav" + File.separator + "tempDir"
                            + File.separator;
                } else {
                    tempDir = tempDir + File.separator;
                }
                file = new File(tempDir);
                if ( !file.exists()) {
                    file.mkdirs();
                }

                file = new File(fileDir + fileName);

                compressedName = fileName;
                compressedDir = fileDir;

                if (unzip) {
                    int totalBytesRead = 0;

                    try {
                        fis = new FileInputStream(file);
                    } catch (final FileNotFoundException e) {
                        MipavUtil.displayError("File not found exception on fis = new FileInputStream(file) for "
                                + fileName);
                        return null;
                    }

                    try {
                        zin = new ZipInputStream(new BufferedInputStream(fis));
                    } catch (final Exception e) {
                        MipavUtil.displayError("Exception on ZipInputStream for " + fileName);
                        return null;
                    }

                    fileName = fileName.substring(0, index);
                    fileDir = tempDir;
                    uncompressedName = fileDir + fileName;
                    try {
                        out = new FileOutputStream(uncompressedName);
                    } catch (final IOException e) {
                        MipavUtil.displayError("IOException on FileOutputStream for " + uncompressedName);
                        return null;
                    }
                    final byte[] buffer = new byte[256];

                    try {
                        while (zin.getNextEntry() != null) {
                            while (true) {

                                bytesRead = zin.read(buffer);

                                if (bytesRead == -1) {
                                    break;
                                }

                                totalBytesRead += bytesRead;
                                out.write(buffer, 0, bytesRead);

                            }
                        } // while (zin.getNextEntry() != null)
                    } catch (final IOException e) {
                        MipavUtil.displayError("IOException in loop reading entries");
                        return null;
                    }

                    try {
                        out.close();
                    } catch (final IOException e) {
                        MipavUtil.displayError("IOException on out.close for " + uncompressedName);
                        return null;
                    }
                } // if (unzip)
                else if (gunzip) {
                    int totalBytesRead = 0;

                    try {
                        fis = new FileInputStream(file);
                    } catch (final FileNotFoundException e) {
                        MipavUtil.displayError("File not found exception on fis = new FileInputStream(file) for "
                                + fileName);
                        return null;
                    }

                    try {
                        gzin = new GZIPInputStream(new BufferedInputStream(fis));
                    } catch (final IOException e) {
                        MipavUtil.displayError("IOException on GZIPInputStream for " + fileName);
                        return null;
                    }

                    fileName = fileName.substring(0, index);
                    fileDir = tempDir;
                    uncompressedName = fileDir + fileName;
                    try {
                        out = new FileOutputStream(uncompressedName);
                    } catch (final IOException e) {
                        MipavUtil.displayError("IOException on FileOutputStream for " + uncompressedName);
                        return null;
                    }
                    final byte[] buffer = new byte[256];

                    while (true) {
                        try {
                            bytesRead = gzin.read(buffer);
                        } catch (final IOException e) {
                            MipavUtil.displayError("IOException on gzin.read(buffer) for " + uncompressedName);
                            return null;
                        }

                        if (bytesRead == -1) {
                            break;
                        }

                        totalBytesRead += bytesRead;
                        try {
                            out.write(buffer, 0, bytesRead);
                        } catch (final IOException e) {
                            MipavUtil.displayError("IOException on out.write for " + uncompressedName);
                            return null;
                        }
                    }

                    try {
                        out.close();
                    } catch (final IOException e) {
                        MipavUtil.displayError("IOException on out.close for " + uncompressedName);
                        return null;
                    }
                } // if (gunzip)
                else if (bz2unzip) {
                    int totalBytesRead = 0;

                    try {
                        fis = new FileInputStream(file);
                    } catch (final FileNotFoundException e) {
                        MipavUtil.displayError("File not found exception on fis = new FileInputStream(file) for "
                                + fileName);
                        return null;
                    }

                    try {
                        fis.read();
                    } catch (final IOException e) {
                        MipavUtil.displayError("IOException on fis.read() trying to read byte B");
                        return null;
                    }

                    try {
                        fis.read();
                    } catch (final IOException e) {
                        MipavUtil.displayError("IOException on fis.read() trying to read byte Z");
                        return null;
                    }

                    try {
                        bz2in = new CBZip2InputStream(new BufferedInputStream(fis));
                    } catch (final Exception e) {
                        MipavUtil.displayError("Exception on CBZip2InputStream for " + fileName);
                        return null;
                    }

                    fileName = fileName.substring(0, index);
                    fileDir = tempDir;
                    uncompressedName = fileDir + fileName;
                    try {
                        out = new FileOutputStream(uncompressedName);
                    } catch (final IOException e) {
                        MipavUtil.displayError("IOException on FileOutputStream for " + uncompressedName);
                        return null;
                    }
                    final byte[] buffer = new byte[256];

                    while (true) {
                        try {
                            bytesRead = bz2in.read(buffer);
                        } catch (final IOException e) {
                            MipavUtil.displayError("IOException on bz2in.read(buffer) for " + uncompressedName);
                            return null;
                        }

                        if (bytesRead == -1) {
                            break;
                        }

                        totalBytesRead += bytesRead;
                        try {
                            out.write(buffer, 0, bytesRead);
                        } catch (final IOException e) {
                            MipavUtil.displayError("IOException on out.write for " + uncompressedName);
                            return null;
                        }
                    }

                    try {
                        out.close();
                    } catch (final IOException e) {
                        MipavUtil.displayError("IOException on out.close for " + uncompressedName);
                        return null;
                    }
                } // else if (bz2unzip)

            }

        }
        if (niftiCompressed) {
            fileType = FileUtility.NIFTI;
        } else {
            boolean zerofunused[] = new boolean[1];
            fileType = FileUtility.getFileType(fileName, fileDir, false, quiet, zerofunused); // set the fileType

            if (fileType == FileUtility.ERROR) {
                return null;
            }

            if (fileType == FileUtility.UNDEFINED && isQuiet()) {
                System.err.println("FileIO read: Unable to get file type from suffix");
                return null;
            }

            if (fileType == FileUtility.UNDEFINED) { // if image type not defined by extension, popup
                fileType = getFileType(); // dialog to get user to define image type
                userDefinedFileType = fileType;
                if (fileName.indexOf(".") != -1) {
                    userDefinedSuffix = "." + fileName.split("\\.")[1];
                }
            }

            fileType = FileIO.chkMultiFile(fileType, multiFile); // for multifile support...
        }

        try {

            switch (fileType) {

                case FileUtility.BMP:
                    image = readBMP(fileName, fileDir, one);
                    break;
                    
                case FileUtility.BMP_MULTIFILE:
                    image = readBMPMulti(fileName, fileDir, one);
                    break;
            
                case FileUtility.LIFF:
                    image = readLIFF(fileName, fileDir, one);
                    break;

                case FileUtility.MATLAB:
                    image = readMATLAB(fileName, fileDir, one);
                    break;

                case FileUtility.TIFF:
                    image = readTiff(fileName, fileDir, one);
                    break;

                case FileUtility.TIFF_MULTIFILE:
                    image = readTiffMulti(fileName, fileDir);
                    break;

                case FileUtility.COR:
                    image = readCOR(fileName, fileDir);
                    break;

                case FileUtility.BRUKER:
                    image = readBRUKER(fileName, fileDir, one);
                    break;

                case FileUtility.LSM:
                    image = readLSM(fileName, fileDir, secondAddress, one);
                    break;

                case FileUtility.LSM_MULTIFILE:
                    image = readLSMMulti(fileName, fileDir);
                    break;

                case FileUtility.STK:
                    image = readSTK(fileName, fileDir, one);
                    break;

                case FileUtility.AVI:
                    image = readAvi(fileName, fileDir, one, false);
                    break;

                case FileUtility.QT:
                    image = readAvi(fileName, fileDir, one, true);
                    break;

                case FileUtility.RAW:
                    image = readRaw(fileName, fileDir, fileInfo);
                    break;

                case FileUtility.RAW_MULTIFILE:
                    image = readRawMulti(fileName, fileDir, fileInfo);
                    break;

                case FileUtility.ANALYZE:
                    image = readAnalyze(fileName, fileDir, one);
                    break;

                case FileUtility.SIEMENSTEXT:
                    image = readSiemensText(fileName, fileDir, false);
                    break;

                case FileUtility.MGH:
                    image = readMGH(fileName, fileDir, one);
                    break;

                case FileUtility.NIFTI:
                    if (niftiCompressed) {
                        image = readNIFTI(fileName, fileDir, one, true);
                        image.setImageName(fileName.substring(0, fileName.lastIndexOf(".")), false);
                    } else {
                        image = readNIFTI(fileName, fileDir, one, false);
                    }

                    break;

                case FileUtility.NRRD:
                    image = readNRRD(fileName, fileDir, one);
                    break;

                case FileUtility.SPM:
                    image = readSPM(fileName, fileDir, one);
                    break;

                case FileUtility.CHESHIRE:
                    image = readCheshire(fileName, fileDir);
                    break;

                case FileUtility.ANALYZE_MULTIFILE:
                    image = readAnalyzeMulti(fileName, fileDir);
                    break;

                case FileUtility.NIFTI_MULTIFILE:
                    image = readNIFTIMulti(fileName, fileDir);
                    break;

                case FileUtility.DICOM:
                    if ( !multiFile) {
                        this.fileDir = fileDir;
                        image = readDicom(fileName, new String[] {fileName.trim()}, false);
                    } else {
                        image = readDicom(fileName, FileUtility.getFileList(fileDir, fileName, quiet), true);
                    }

                    break;

                case FileUtility.MEDIVISION:
                    image = readMedVision(fileName, fileDir);
                    break;

                case FileUtility.MAP:
                    image = readMap(fileName, fileDir);
                    break;

                case FileUtility.JIMI:
                    image = readJimi(fileName, fileDir, multiFile);
                    break;

                case FileUtility.MINC:
                    image = readMinc(fileName, fileDir, one);
                    break;
                case FileUtility.MINC_HDF:
                    image = readMincHDF(fileName, fileDir, one);
                    break;
                case FileUtility.AFNI:
                    image = readAfni(fileName, fileDir, loadB);
                    break;

                case FileUtility.ICS:
                    image = readICS(fileName, fileDir);
                    break;

                case FileUtility.INTERFILE:
                    image = readInterfile(fileName, fileDir, one);
                    break;

                case FileUtility.BIORAD:
                    image = readBioRad(fileName, fileDir, one);
                    break;

                case FileUtility.FITS:
                    image = readFits(fileName, fileDir, one);
                    break;

                case FileUtility.DM3:
                    image = readDM3(fileName, fileDir, one);
                    break;

                case FileUtility.TMG:
                    image = readTMG(fileName, fileDir);
                    break;

                case FileUtility.MRC:
                    image = readMRC(fileName, fileDir);
                    break;

                case FileUtility.BFLOAT:
                    image = readBFLOAT(fileName, fileDir, one);
                    break;

                case FileUtility.MAGNETOM_VISION:
                    image = readMagnetomVision(fileName, fileDir);
                    break;

                case FileUtility.MAGNETOM_VISION_MULTIFILE:
                    image = readMagnetomVisionMulti(fileName, fileDir);
                    break;

                case FileUtility.GE_GENESIS:
                    image = readGEGenesis5X(fileName, fileDir);
                    break;

                case FileUtility.GE_GENESIS_MULTIFILE:
                    image = readGEGenesis5XMulti(fileName, fileDir);
                    break;

                case FileUtility.GE_SIGNA4X:
                    image = readGESigna4X(fileName, fileDir);
                    break;

                case FileUtility.GE_SIGNA4X_MULTIFILE:
                    image = readGESigna4XMulti(fileName, fileDir);
                    break;

                case FileUtility.MICRO_CAT:
                    image = readMicroCat(fileName, fileDir, one);
                    break;

                case FileUtility.XML:
                    image = readXML(fileName, fileDir, one, !quiet);
                    break;

                case FileUtility.XML_MULTIFILE:
                    image = readXMLMulti(fileName, fileDir);
                    break;

                case FileUtility.PARREC:
                    image = readPARREC(fileName, fileDir, one);
                    break;

                case FileUtility.ZVI:
                    image = readZVI(fileName, fileDir, one);
                    break;

                case FileUtility.JP2:
                    image = readJpeg2000(fileName, fileDir, one);
                    break;

                case FileUtility.VISTA:
                    image = readVista(fileName, fileDir, one);
                    break;

                default:
                    return null;
            }

            if (unzip || gunzip || bz2unzip) {
                if ( !niftiCompressed) {
                    // Delete the input uncompressed file
                    File uncompressedFile;
                    uncompressedFile = new File(uncompressedName);
                    try {
                        uncompressedFile.delete();
                    } catch (final SecurityException sc) {
                        MipavUtil.displayError("Security error occurs while trying to delete " + uncompressedName);
                    }

                    // set the file name and dir back to the original file(s) when we had to do a temp decompression
                    if (image != null) {
                        String compressionExt = "";
                        if (unzip) {
                            compressionExt = ".zip";
                        } else if (gunzip) {
                            compressionExt = ".gz";
                        } else if (bz2unzip) {
                            compressionExt = ".bz2";
                        }
                        final FileInfoBase[] fInfos = image.getFileInfo();
                        for (final FileInfoBase element : fInfos) {
                            element.setFileDirectory(compressedDir);
                            element.setFileName(element.getFileName() + compressionExt);
                            element.setFileSuffix(element.getFileSuffix() + compressionExt);
                        }
                    }
                    fileName = compressedName;
                    fileDir = compressedDir;
                }
            }

            if (image != null) {
                if (ProvenanceRecorder.getReference().getRecorderStatus() == ProvenanceRecorder.RECORDING) {
                    final int idx = fileName.lastIndexOf(".");
                    String fName;
                    if (idx == -1) {
                        fName = new String(fileName);
                    } else {
                        fName = fileName.substring(0, idx);
                    }
                    if (new File(fileDir + File.separator + fName + ".xmp").exists()) {
                        try {
                            final FileDataProvenance fdp = new FileDataProvenance(fName + ".xmp", fileDir, image
                                    .getProvenanceHolder());

                            fdp.readHeader(fName + ".xmp", fileDir, Preferences.DATA_PROVENANCE_SCHEMA);

                        } catch (final Exception e) {
                            e.printStackTrace();
                        }
                    }
                }

                // tell mipav system data provenance to record this opening of an image
                ProvenanceRecorder.getReference().addLine(new ActionOpenImage(image));

                if (progressBar != null) {
                    progressBar.dispose();
                }

                if ( (image.getType() == ModelStorageBase.COMPLEX) || (image.getType() == ModelStorageBase.DCOMPLEX)) {
                    image.calcMinMaxMag(Preferences.is(Preferences.PREF_LOGMAG_DISPLAY));
                } else {
                    image.calcMinMax();
                    // image.setImageDirectory(fileDir);
                }

                // if file type was a new user defined file type, then we need to save its association
                // to the preferences if its not already there.
                // we also need to save this pref as part of both the userDefinedFileTypes and
                // the userDefinedFileTypes_textField
                if (userDefinedSuffix != null) {
                    final String association = userDefinedSuffix + Preferences.DEFINITION_SEPARATOR
                            + userDefinedFileType;
                    boolean isPresent = false;

                    if (Preferences.getProperty(Preferences.PREF_USER_FILETYPE_ASSOC) != null) {

                        if ( !Preferences.getProperty(Preferences.PREF_USER_FILETYPE_ASSOC).trim().equals("")) {
                            final String[] userDefinedFileTypeAssociations = Preferences.getProperty(
                                    Preferences.PREF_USER_FILETYPE_ASSOC).split(Preferences.ITEM_SEPARATOR);

                            for (final String element : userDefinedFileTypeAssociations) {

                                if (userDefinedSuffix.equals(element.split(Preferences.DEFINITION_SEPARATOR)[0])) {
                                    isPresent = true;
                                }
                            }

                            if ( !isPresent) {
                                Preferences.setProperty(Preferences.PREF_USER_FILETYPE_ASSOC, Preferences
                                        .getProperty(Preferences.PREF_USER_FILETYPE_ASSOC)
                                        + Preferences.ITEM_SEPARATOR + association);
                                setUserDefinedFileTypesPref(userDefinedSuffix);
                                setUserDefinedFileTypes_textFieldPref(userDefinedSuffix);
                            }

                        } else {
                            Preferences.setProperty(Preferences.PREF_USER_FILETYPE_ASSOC, association);
                            setUserDefinedFileTypesPref(userDefinedSuffix);
                            setUserDefinedFileTypes_textFieldPref(userDefinedSuffix);
                        }
                    } else {
                        Preferences.setProperty(Preferences.PREF_USER_FILETYPE_ASSOC, association);
                        setUserDefinedFileTypesPref(userDefinedSuffix);
                        setUserDefinedFileTypes_textFieldPref(userDefinedSuffix);
                    }
                }
            }
        } catch (final Exception error) {

            if (progressBar != null) {
                progressBar.dispose();
            }

            error.printStackTrace();

            if ( !quiet) {
                MipavUtil.displayError("Unable to load image.  See debug window for more details.");
            }

            Preferences.debug("Error while loading " + fileDir + fileName + ".\n" + error + "\n",
                    Preferences.DEBUG_FILEIO);

            return null;
        }

        return image;
    }

    /**
     * Reads file, determines file type, and calls a read function specific to the file. That read function returns an
     * image and this function stores it in ModelImage. File is not multi file, file info is not previously known,
     * there's no "second address" for AFNI, and this is not an image B. This is just one image regardless of how many
     * slices there are; the middle slice of a 3D dataset will be displayed.
     * 
     * @param fileName File name where image is located.
     * @param fileDir File directory where image is located.
     * 
     * @return The image that was read in from the file.
     */
    public ModelImage readOneImage(final String fileName, final String fileDir) {
        return readImage(fileName, fileDir, false, null, 0, false, true);
    }

    /**
     * This method examines parameter <i>fileList <i>and loads TIFF files according to the order in the list. It can
     * load 2, 3, or 4 channel images. The method will examine channelMap to determine in which order the ARGB channels
     * should be interleaved.</i></i>
     * 
     * @param fileList File[] - the list of File objects, preordered
     * @param numChannels int - the number of channels the result image is to have
     * @param channelMap int[] - a mapping of channels (ARGB) to positions. For example, a channelMap parameter of {2,
     *            1, 0, 3} means the first channel will be G, the second will be R, the third will be A and the last
     *            will be B.
     * @param showOrderedProgressBar boolean - This parameter is used to control whether the local progress bar is
     *            shown. Note this is different than FileIO's global progress bar. The reason for the difference is
     *            because this method uses FileIO's readOneImage() method. That method uses the global progress bar. Its
     *            useless to have two progress bars, and FileIO isn't set up in a way that allows this method to
     *            directly control the global progress bar, hence the need for its own local one.
     * @param subsampleDimension - the dimensions of the result image if subsampling is desired. To skip subsampling,
     *            this parameter should be null
     * @param forceUBYTE boolean - force the image to be constructed with an unsigned byte data type
     * 
     * @return ModelImage
     */
    public ModelImage readOrderedARGB(final File[] fileList, final int numChannels, int[] channelMap,
            final boolean showOrderedProgressBar, final Dimension subsampleDimension, final boolean forceUBYTE) {
        ModelImage modelImageTemp = null;
        ModelImage modelImageResult = null;
        float[] oneSliceBuffer;
        float[] resultImageBuffer;
        int[] imageExtents;
        int sliceSize;

        try // read one image in order to get image dimensions
        {
            modelImageTemp = readOneImage(fileList[0].getName(), fileList[0].getParentFile().getAbsolutePath()
                    + File.separator);

            if (subsampleDimension != null) {
                modelImageTemp = FileIO.subsample(modelImageTemp, subsampleDimension);
            }

            imageExtents = modelImageTemp.getExtents();

            sliceSize = imageExtents[0] * imageExtents[1]; // width * height

            resultImageBuffer = new float[sliceSize * 4]; // the * 4 is because there are 4 channels - ARGB

            oneSliceBuffer = new float[sliceSize];
        } catch (final Exception ioe) {
            ioe.printStackTrace();

            return null;
        }

        createProgressBar(null, "files", FileIO.FILE_READ);

        // allocate memory for the result ModelImage.
        int[] extents = {imageExtents[0], imageExtents[1], fileList.length / numChannels};

        if (modelImageTemp.getType() == ModelStorageBase.USHORT) {
            modelImageResult = new ModelImage(ModelStorageBase.ARGB_USHORT, extents, modelImageTemp.getImageName());
        } else if (modelImageTemp.getType() == ModelStorageBase.FLOAT) {
            modelImageResult = new ModelImage(ModelStorageBase.ARGB_FLOAT, extents, modelImageTemp.getImageName());
        } else {
            modelImageResult = new ModelImage(ModelStorageBase.ARGB, extents, modelImageTemp.getImageName());
        }

        extents = null;
        modelImageTemp.disposeLocal(false);

        for (int n = 0; n < fileList.length; n++) {
            final int channel = channelMap[n % numChannels];
            final int currentSlice = (n / numChannels);

            if (fileList[n].exists()) {

                try {
                    modelImageTemp = readOneImage(fileList[n].getName(), fileList[n].getParentFile().getAbsolutePath()
                            + File.separator);

                    if (subsampleDimension != null) {
                        modelImageTemp = FileIO.subsample(modelImageTemp, subsampleDimension);
                    }

                    modelImageTemp.exportData(0, oneSliceBuffer.length, oneSliceBuffer); // export one slice at a
                    // time
                    // to result ModelImage

                    for (int i = 0; i < oneSliceBuffer.length; i++) {
                        resultImageBuffer[ (i * 4) + channel] = oneSliceBuffer[i]; // arrange interleaved pixels
                    }

                    modelImageResult.importData(currentSlice * sliceSize * 4, resultImageBuffer, false);
                    FileIO.copyResolutions(modelImageTemp, modelImageResult, currentSlice);

                    modelImageTemp.disposeLocal(false);
                } catch (final IOException ioe) {
                    ioe.printStackTrace();

                    break;
                }
            } else {
                Preferences.debug("File does not exist: " + fileList[n].getName() + "\n", Preferences.DEBUG_FILEIO);
            }

            progressBar.updateValue((int) ( ( (n + 1) / (float) fileList.length) * 100), false);
        }

        modelImageResult.calcMinMax();

        progressBar = null;
        resultImageBuffer = null;
        oneSliceBuffer = null;
        channelMap = null;
        modelImageTemp = null;

        if ( (forceUBYTE == true) && (modelImageResult.getType() != ModelStorageBase.ARGB)) {
            return FileIO.convertToARGB(modelImageResult);
        }

        return modelImageResult;
    }

    /**
     * This method will load a group of files in the order of <i>fileList<i>. The result will be a grayscale
     * ModelImage. </i></i>
     * 
     * @param fileList File[] - list of File objects, preordered
     * @param showLocalProgressBar boolean - This parameter is used to control whether the local progress bar is shown.
     *            Note this is different than FileIO's global progress bar. The reason for the difference is because
     *            this method uses FileIO's readOneImage() method. That method uses the global progress bar. Its useless
     *            to have two progress bars, and FileIO isn't set up in a way that allows this method to directly
     *            control the global progress bar, hence the need for its own local one.
     * @param subsampleDimension - the dimensions of the result image if subsampling is desired. To skip subsampling,
     *            this parameter should be null
     * @param forceUBYTE boolean - force the image to be constructed with an unsigned byte data type
     * 
     * @return ModelImage
     */
    public ModelImage readOrderedGrayscale(final File[] fileList, final boolean showLocalProgressBar,
            final Dimension subsampleDimension, final boolean forceUBYTE) {
        ModelImage modelImageTemp = null;
        ModelImage modelImageResult = null;
        float[] oneSliceBuffer;

        createProgressBar(null, "files", FileIO.FILE_READ);

        try {

            // read one image so we can get extents
            modelImageTemp = readOneImage(fileList[0].getName(), fileList[0].getParentFile().getAbsolutePath()
                    + File.separator);

            if (subsampleDimension != null) // subsample the image if we have subsampling dimensions
            {
                modelImageTemp = FileIO.subsample(modelImageTemp, subsampleDimension);
            }

            // create a buffer to hold exchange image data between temp and result images
            oneSliceBuffer = new float[modelImageTemp.getExtents()[0] * modelImageTemp.getExtents()[1]];

            // the first slice has already been read. instead of re-reading it in the loop, export to buffer and save
            // an iteration
            modelImageTemp.exportData(0, oneSliceBuffer.length, oneSliceBuffer);

            // the result image's dimensions (possibly subsampled dimensions)
            int[] extents = {modelImageTemp.getExtents()[0], modelImageTemp.getExtents()[1], fileList.length};

            modelImageResult = new ModelImage(modelImageTemp.getType(), extents, modelImageTemp.getImageName());
            FileIO.copyResolutions(modelImageTemp, modelImageResult, 0); // save the resolutions from the file info
            // structure

            extents = null;
            modelImageTemp.disposeLocal(false);

            // import first slice to result image from modelImageTemp
            modelImageResult.importData(0, oneSliceBuffer, false);

            progressBar.updateValue((int) ( (1.0f / fileList.length) * 100), false);

            for (int i = 1; i < fileList.length; i++) {

                if (fileList[i].exists()) {

                    try {

                        // read images one slice at a time
                        modelImageTemp = readOneImage(fileList[i].getName(), fileList[i].getParentFile()
                                .getAbsolutePath()
                                + File.separator);

                        if (subsampleDimension != null) // subsample if we have subsampling dimensions
                        {
                            modelImageTemp = FileIO.subsample(modelImageTemp, subsampleDimension);
                        }

                        modelImageTemp.exportData(0, oneSliceBuffer.length, oneSliceBuffer);
                        FileIO.copyResolutions(modelImageTemp, modelImageResult, i);

                        modelImageResult.importData(i * oneSliceBuffer.length, oneSliceBuffer, false);
                    } catch (final IOException ioe) {
                        ioe.printStackTrace();

                        break;
                    } finally {
                        modelImageTemp.disposeLocal(false);
                    }
                } else {
                    Preferences.debug("File does not exist: " + fileList[i].getName() + "\n", Preferences.DEBUG_FILEIO);
                }

                progressBar.updateValue((int) ( ((float) (i + 1) / (float) fileList.length) * 100), false);
            }

            modelImageResult.calcMinMax();

            if ( (forceUBYTE == true) && (modelImageResult.getType() != ModelStorageBase.UBYTE)) {
                oneSliceBuffer = null;

                return FileIO.convertToUBYTE(modelImageResult);
            }

            return modelImageResult;
        } catch (final Exception ioe) {
            ioe.printStackTrace();

            return null;
        } finally {

            if (progressBar != null) {
                progressBar.dispose();
            }

            modelImageTemp.disposeLocal();
            modelImageTemp = null;
            oneSliceBuffer = null;
        }
    }

    /**
     * This method will load a group of files in the order of <i>fileList<i>. The result will be a grayscale
     * ModelImage. </i></i>
     * 
     * @param fileList File[] - list of File objects, preordered
     * @param showLocalProgressBar boolean - This parameter is used to control whether the local progress bar is shown.
     *            Note this is different than FileIO's global progress bar. The reason for the difference is because
     *            this method uses FileIO's readOneImage() method. That method uses the global progress bar. Its useless
     *            to have two progress bars, and FileIO isn't set up in a way that allows this method to directly
     *            control the global progress bar, hence the need for its own local one.
     * @param subsampleDimension - the dimensions of the result image if subsampling is desired. To skip subsampling,
     *            this parameter should be null
     * @param forceUBYTE boolean - force the image to be constructed with an unsigned byte data type
     * 
     * @return ModelImage
     */
    public ModelImage readOrderedGrayscale(final File[] fileList, final boolean showLocalProgressBar,
            final Dimension subsampleDimension, final boolean forceUBYTE, final int numSlices, final int numTimePoints) {
        ModelImage modelImageTemp = null;
        ModelImage modelImageResult = null;
        int [] subsampledExtents = null;
        int [] padExtents = null;
        ModelImage modelImageSubsample = null;
        float[] oneSliceBuffer;
        FileDicomTagTable[] childTagTables = null;
        boolean is3DDicom = false;
        createProgressBar(null, "files", FileIO.FILE_READ);
        float[] resolutions = null;
        int[] extents = null;
        FileInfoBase firstFileInfo = null;

        try {

            // read one image so we can get extents
            modelImageTemp = readOneImage(fileList[0].getName(), fileList[0].getParentFile().getAbsolutePath()
                    + File.separator);

            if (subsampleDimension != null) // subsample the image if we have subsampling dimensions
            {
                modelImageTemp = FileIO.subsample(modelImageTemp, subsampleDimension);
            }
            
            // create a buffer to hold exchange image data between temp and result images
            oneSliceBuffer = new float[modelImageTemp.getExtents()[0] * modelImageTemp.getExtents()[1]];

            // the first slice has already been read. instead of re-reading it in the loop, export to buffer and save
            // an iteration
            modelImageTemp.exportData(0, oneSliceBuffer.length, oneSliceBuffer);
            
            System.out.println("Free: "+MipavUtil.getFreeHeapMemory());
            
            // the result image's dimensions (possibly subsampled dimensions)

            if (numTimePoints > 1) {
                extents = new int[4];
                extents[0] = modelImageTemp.getExtents()[0];
                extents[1] = modelImageTemp.getExtents()[1];
                extents[2] = numSlices;
                extents[3] = numTimePoints;
            } else {
                extents = new int[3];
                extents[0] = modelImageTemp.getExtents()[0];
                extents[1] = modelImageTemp.getExtents()[1];
                extents[2] = fileList.length;
            }

            modelImageResult = new ModelImage(modelImageTemp.getType(), extents, modelImageTemp.getImageName());
            firstFileInfo = modelImageTemp.getFileInfo(0);
            if (modelImageTemp.getFileInfo(0).getFileFormat() == FileUtility.DICOM) {
                if (numTimePoints <= 1) {
                    is3DDicom = true;
                    childTagTables = new FileDicomTagTable[fileList.length - 1];
                    final FileInfoDicom oldDicomInfo = (FileInfoDicom) modelImageTemp.getFileInfo(0);
                    final FileInfoDicom destFileInfo = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo
                            .getFileDirectory(), oldDicomInfo.getFileFormat());
                    (destFileInfo).setVr_type(oldDicomInfo.getVr_type());
                    (destFileInfo).getTagTable().importTags(oldDicomInfo);
                    modelImageResult.setFileInfo(destFileInfo, 0);
                }

            }
            if (numTimePoints > 1) {
                resolutions = new float[4];
                resolutions[0] = modelImageTemp.getFileInfo(0).getResolution(0);
                resolutions[1] = modelImageTemp.getFileInfo(0).getResolution(1);
                resolutions[2] = 1.0f;
                resolutions[3] = 1.0f;

            } else {
                resolutions = new float[3];
                resolutions[0] = modelImageTemp.getFileInfo(0).getResolution(0);
                resolutions[1] = modelImageTemp.getFileInfo(0).getResolution(1);
                resolutions[2] = 1.0f;

            }
            // FileIO.copyResolutions(modelImageTemp, modelImageResult, 0); // save the resolutions from the file info
            // structure

            modelImageTemp.disposeLocal(false);

            // import first slice to result image from modelImageTemp
            modelImageResult.importData(0, oneSliceBuffer, false);

            progressBar.updateValue((int) ( (1.0f / fileList.length) * 100), false);

            for (int i = 1; i < fileList.length; i++) {
                System.out.println(i);
                if (fileList[i].exists()) {

                    try {

                        // read images one slice at a time
                        modelImageTemp = readOneImage(fileList[i].getName(), fileList[i].getParentFile()
                                .getAbsolutePath()
                                + File.separator);

                        if (subsampleDimension != null) // subsample if we have subsampling dimensions
                        {
                        	if (i == 1) {
                        		subsampledExtents = new int[] {subsampleDimension.getSize().width,
                                        subsampleDimension.getSize().height};
                        		padExtents = modelImageTemp.getExtents();
                        		
                                modelImageSubsample = new ModelImage(modelImageTemp.getType(), new int[] {
                                        subsampleDimension.getSize().width, subsampleDimension.getSize().height},
                                        modelImageTemp.getImageName() + "_subsampled");	
                        	} // if (i == 1)
                        	AlgorithmSubsample algorithmSubsample = new AlgorithmSubsample(modelImageTemp, modelImageSubsample,
                                    subsampledExtents, padExtents, new float[] {1.0f, 1.0f, 1.0f}, false, false, null, false);
                            algorithmSubsample.run();
                            algorithmSubsample.finalize();
                            algorithmSubsample = null;
                        } // if (subsampleDimension != null)
                        else {
                        	modelImageSubsample = modelImageTemp;
                        }
                        System.out.println("Free: "+MipavUtil.getFreeHeapMemory());
                        //System.gc();
                        modelImageSubsample.exportData(0, oneSliceBuffer.length, oneSliceBuffer);
                        FileIO.copyResolutions(modelImageSubsample, modelImageResult, i);
                        modelImageResult.importData(i * oneSliceBuffer.length, oneSliceBuffer, false);
                        
                        if (is3DDicom) {
                            final FileInfoDicom oldDicomInfo = (FileInfoDicom) modelImageSubsample.getFileInfo(0);
                            final FileInfoDicom destFileInfo = new FileInfoDicom(oldDicomInfo.getFileName(),
                                    oldDicomInfo.getFileDirectory(), oldDicomInfo.getFileFormat(),
                                    (FileInfoDicom) modelImageResult.getFileInfo(0));
                            (destFileInfo).setVr_type(oldDicomInfo.getVr_type());

                            childTagTables[i - 1] = (destFileInfo).getTagTable();

                            (destFileInfo).getTagTable().importTags(oldDicomInfo);
                            modelImageResult.setFileInfo(destFileInfo, i);
                        }

                    } catch (final IOException ioe) {
                        ioe.printStackTrace();

                        break;
                    } finally {
                    	/*if ((i % 50) == 0) {
                    		modelImageTemp.disposeLocal(true);
                    	}*/
                    	{
                            modelImageTemp.disposeLocal(false);
                    	}
                    }
                } else {
                    Preferences.debug("File does not exist: " + fileList[i].getName() + "\n", Preferences.DEBUG_FILEIO);
                }

                progressBar.updateValue((int) ( ((float) (i + 1) / (float) fileList.length) * 100), false);

            }
            if (modelImageSubsample != null) {
            	modelImageSubsample.disposeLocal(false);
            }
            if (is3DDicom) {

                ((FileInfoDicom) modelImageResult.getFileInfo(0)).getTagTable().attachChildTagTables(childTagTables);
            }

            modelImageResult.calcMinMax();

            if ( (forceUBYTE == true) && (modelImageResult.getType() != ModelStorageBase.UBYTE)) {
                oneSliceBuffer = null;

                return FileIO.convertToUBYTE(modelImageResult);
            }

            for (int i = 0; i < fileList.length; i++) {
                modelImageResult.getFileInfo(i).setExtents(extents);
                modelImageResult.getFileInfo(i).setResolutions(resolutions);
                modelImageResult.getFileInfo(i).setAxisOrientation(firstFileInfo.getAxisOrientation());
                modelImageResult.getFileInfo(i).setDataType(firstFileInfo.getDataType());
                modelImageResult.getFileInfo(i).setEndianess(firstFileInfo.getEndianess());
                modelImageResult.getFileInfo(i).setImageOrientation(firstFileInfo.getImageOrientation());

                modelImageResult.getFileInfo(i).setMin(modelImageResult.getMin());
                modelImageResult.getFileInfo(i).setMax(modelImageResult.getMax());
                modelImageResult.getFileInfo(i).setModality(firstFileInfo.getModality());

            }

            return modelImageResult;
        } catch (final Exception ioe) {
            ioe.printStackTrace();

            return null;
        } finally {

            if (progressBar != null) {
                progressBar.dispose();
            }

            modelImageTemp.disposeLocal();
            modelImageTemp = null;
            oneSliceBuffer = null;
        }
    }

    public ModelImage readOrderedGrayscale2(final File[] fileList, final boolean showLocalProgressBar,
            final Dimension subsampleDimension, final boolean forceUBYTE, final int numSlices, final int numTimePoints) {
        ModelImage modelImageResult = null;
        ModelImage modelImageTemp = null;
        float[] oneSliceBuffer;
        int[] extents = null;
        float[] resolutions = null;
        boolean isDicom = false;
        final FileInfoBase[] destFileInfo = new FileInfoBase[fileList.length];

        try {

            // read one image so we can get extents
            modelImageTemp = readOneImage(fileList[0].getName(), fileList[0].getParentFile().getAbsolutePath()
                    + File.separator);

            if (modelImageTemp.getFileInfo(0).getFileFormat() == FileUtility.DICOM) {
                isDicom = true;
            }

            if (subsampleDimension != null) // subsample the image if we have subsampling dimensions
            {
                modelImageTemp = FileIO.subsample(modelImageTemp, subsampleDimension);
            }

            // create a buffer to hold exchange image data between temp and result images
            oneSliceBuffer = new float[modelImageTemp.getExtents()[0] * modelImageTemp.getExtents()[1]];

            // the first slice has already been read. instead of re-reading it in the loop, export to buffer and
                // save
            // an iteration
            modelImageTemp.exportData(0, oneSliceBuffer.length, oneSliceBuffer);

            if (numTimePoints > 1) {
                extents = new int[4];
                extents[0] = modelImageTemp.getExtents()[0];
                extents[1] = modelImageTemp.getExtents()[1];
                extents[2] = numSlices;
                extents[3] = numTimePoints;
            } else {
                extents = new int[3];
                extents[0] = modelImageTemp.getExtents()[0];
                extents[1] = modelImageTemp.getExtents()[1];
                extents[2] = fileList.length;
            }

            if (numTimePoints > 1) {
                resolutions = new float[4];
                resolutions[0] = modelImageTemp.getFileInfo(0).getResolution(0);
                resolutions[1] = modelImageTemp.getFileInfo(0).getResolution(1);
                resolutions[2] = 1.0f;
                resolutions[3] = 1.0f;
            } else {
                resolutions = new float[3];
                resolutions[0] = modelImageTemp.getFileInfo(0).getResolution(0);
                resolutions[1] = modelImageTemp.getFileInfo(0).getResolution(1);
                resolutions[2] = 1.0f;
            }

            if (isDicom) {
                final FileInfoDicom oldDicomInfo = (FileInfoDicom) modelImageTemp.getFileInfo(0);
                destFileInfo[0] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                        oldDicomInfo.getFileFormat());
            }

            modelImageResult = new ModelImage(modelImageTemp.getType(), extents, modelImageTemp.getImageName());

            modelImageTemp.disposeLocal(false);

            // import first slice to result image from modelImageTemp
            modelImageResult.importData(0, oneSliceBuffer, false);

            for (int i = 1; i < fileList.length; i++) {
                System.out.println(i);
                if (fileList[i].exists()) {

                    // read images one slice at a time
                    modelImageTemp = readOneImage(fileList[i].getName(), fileList[i].getParentFile().getAbsolutePath()
                            + File.separator);

                    if (subsampleDimension != null) // subsample if we have subsampling
                                                                            // dimensions
                    {
                        modelImageTemp = FileIO.subsample(modelImageTemp, subsampleDimension);
                    }

                    modelImageTemp.exportData(0, oneSliceBuffer.length, oneSliceBuffer);

                }
            }

        } catch (final Exception e) {
            return null;
        }

        return modelImageResult;
    }

    /**
     * Reads a thumbnail image from an XML file. if thumbnail is empty, will returned FileImageXML will be null.
     * 
     * @param name filename
     * @param directory file's directory
     * 
     * @return FileImageXML containing thumbnail or null
     */
    public FileImageXML readXMLThumbnail(final String name, final String directory) {
        final FileImageXML xmlTemp = new FileImageXML(name, directory);
        float[][] res = null;

        try {
            final TalairachTransformInfo talairach = new TalairachTransformInfo();
            res = xmlTemp.readHeader(name, directory, talairach);

            if ( (res != null) && (xmlTemp.getThumbnail() != null)) {
                return xmlTemp;
            }
        } catch (final IOException ioex) {
            System.err.println("Got IOException");
        }

        return null;
    }

    /**
     * Sets the directory where the file will be saved or opened.
     * 
     * @param dir String directory
     */
    public void setFileDir(final String dir) {
        this.fileDir = dir;
    }

    /**
     * Sets the LUT.
     * 
     * @param lut the lookup table.
     */
    public void setModelLUT(final ModelLUT lut) {
        this.LUT = lut;
    }

    /**
     * Sets the RGB.
     * 
     * @param rgb lut the lookup table.
     */
    public void setModelRGB(final ModelRGB rgb) {
        this.modelRGB = rgb;
    }

    /**
     * Sets the progress bar (either panel or frame) to be used in image opening to update status.
     * 
     * @param pBar ProgressBarInterface
     */
    public void setPBar(final ProgressBarInterface pBar) {
    // this.pInterface = pBar;
    }

    /**
     * Refers to whether or not the FileIO will send alerts to the user about progress or errors.
     * 
     * @param q Indicates if the output from the methods in this object are to display dialogs interrupting flow in a
     *            locally-defined manner. <code>true</code> indicates that the process is to NOT inform the user and
     *            therefore be
     *            <q>quiet</q>.
     */
    public void setQuiet(final boolean q) {
        quiet = q;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param rawInfo DOCUMENT ME!
     */
    public void setRawImageInfo(final RawImageInfo rawInfo) {
        this.rawInfo = rawInfo;
    }

    /**
     * This method sets the userDefinedFileTypes_textField preference.
     * 
     * @param udefSuffix the user defined suffix
     */

    public void setUserDefinedFileTypes_textFieldPref(final String udefSuffix) {

        if (Preferences.getProperty(Preferences.PREF_USER_FILETYPES_TEXTFIELDS) != null) {

            if (Preferences.getProperty(Preferences.PREF_USER_FILETYPES_TEXTFIELDS).trim().equals("")) {
                Preferences.setProperty(Preferences.PREF_USER_FILETYPES_TEXTFIELDS, "*" + udefSuffix);
            } else {

                // first check to see if its already not there
                final String[] prefTypes = Preferences.getProperty(Preferences.PREF_USER_FILETYPES_TEXTFIELDS).split(
                        ";");
                boolean isPresent = false;

                for (final String element : prefTypes) {
                    String suff = element.split("\\.")[1];
                    suff = "." + suff;

                    if (udefSuffix.equals(suff)) {
                        isPresent = true;
                    }
                }

                if ( !isPresent) {
                    Preferences.setProperty(Preferences.PREF_USER_FILETYPES_TEXTFIELDS, Preferences
                            .getProperty(Preferences.PREF_USER_FILETYPES_TEXTFIELDS)
                            + ";*" + udefSuffix);
                }
            }
        } else {
            Preferences.setProperty(Preferences.PREF_USER_FILETYPES_TEXTFIELDS, "*" + udefSuffix);
        }
    }

    /**
     * This method sets the userDefinedFileTypes preference.
     * 
     * @param udefSuffix the user defined suffix
     */
    public void setUserDefinedFileTypesPref(final String udefSuffix) {

        if (Preferences.getProperty(Preferences.PREF_USER_FILETYPES) != null) {

            if (Preferences.getProperty(Preferences.PREF_USER_FILETYPES).trim().equals("")) {
                Preferences.setProperty(Preferences.PREF_USER_FILETYPES, "*" + udefSuffix);
            } else {

                // first check to see if its already not there
                final String[] prefTypes = Preferences.getProperty(Preferences.PREF_USER_FILETYPES).split(";");
                boolean isPresent = false;

                for (final String element : prefTypes) {
                    String suff = element.split("\\.")[1];
                    suff = "." + suff;

                    if (udefSuffix.equals(suff)) {
                        isPresent = true;
                    }
                }

                if ( !isPresent) {
                    Preferences.setProperty(Preferences.PREF_USER_FILETYPES, Preferences
                            .getProperty(Preferences.PREF_USER_FILETYPES)
                            + "; *" + udefSuffix);
                }
            }
        } else {
            Preferences.setProperty(Preferences.PREF_USER_FILETYPES, "*" + udefSuffix);
        }

    }

    /**
     * Determines file type from the file name and calls different write functions based on the file type. Stores file
     * in the specified file name and directory given by the options. Calls appropriate dialogs if necessary. Supports
     * files of type raw, analyze, and DICOM.
     * 
     * @param image Image to write.
     * @param options Needed info to write this image.
     */
    public void writeImage(final ModelImage image, final FileWriteOptions options) {
        writeImage(image, options, true);
    }

    /**
     * Determines file type from the file name and calls different write functions based on the file type. Stores file
     * in the specified file name and directory given by the options. Calls appropriate dialogs if necessary. Supports
     * files of type raw, analyze, and DICOM.
     * 
     * @param image Image to write.
     * @param options Needed info to write this image.
     * @param bDisplayProgress when true display the progress bar for writing.
     */
    public void writeImage(final ModelImage image, final FileWriteOptions options, final boolean bDisplayProgress) {
        int fileType;
        String suffix;
        int index;
        String ext;
        boolean singleFileNIFTI = false;
        boolean zip = false;
        ZipOutputStream zout;
        boolean gzip = false;
        GZIPOutputStream gzout;
        boolean bz2zip = false;
        CBZip2OutputStream bz2out;
        FileInputStream in;
        byte buf[];
        int len;
        File inputFile;
        FileOutputStream out;
        String inputFileName[] = null;
        String outputFileName[] = null;
        String compressionExt = null;
        int i;
        int numFiles;
        // If true, zero funused fields in analyze write
        boolean zerofunused[] = new boolean[1];

        // set it to quiet mode (no prompting) if the options were
        // created during a script
        if (options.isScript() == true) {
            quiet = true;
        }

        index = options.getFileName().lastIndexOf(".");

        if (index >= 0) {
            ext = options.getFileName().substring(index + 1);
            if (ext.equalsIgnoreCase("zip")) {
                options.setFileName(options.getFileName().substring(0, index));
                zip = true;
                options.setZip(true);
                image.getFileInfo()[0].setCompressionType(FileInfoBase.COMPRESSION_ZIP);
            } else if (ext.equalsIgnoreCase("gz")) {
                options.setFileName(options.getFileName().substring(0, index));
                gzip = true;
                options.setGzip(true);
                image.getFileInfo()[0].setCompressionType(FileInfoBase.COMPRESSION_GZIP);
            } else if (ext.equalsIgnoreCase("bz2")) {
                options.setFileName(options.getFileName().substring(0, index));
                bz2zip = true;
                options.setBz2zip(true);
                image.getFileInfo()[0].setCompressionType(FileInfoBase.COMPRESSION_BZIP2);
            }
        }

        if (options.isSaveAs()) { // if we're doing a save-as op, then try to get the filetype from the name
            fileType = FileUtility.getFileType(options.getFileName(), options.getFileDirectory(), true, quiet, zerofunused);

            // System.err.println("FileType: " + fileType);

            options.setDefault(true); // this would already be set.... hrmm....
        } else { // otherwise, get the file-type from the file-info.

            fileType = image.getFileInfo(0).getFileFormat();
        }
        
        if (fileType == FileUtility.NIFTI) {
            if (options.getNIFTIExtension() != null) {
                String niftiExtension = options.getNIFTIExtension();
                options.setFileName(options.getFileName().substring(0, index) + niftiExtension);
            }
        }

        if (fileType == FileUtility.UNDEFINED) { // if type is still undef, look for user input (when not quiet)
            fileType = options.getFileType(); // get saved file type from options
            options.setSaveAs(true); // can't tell from extension, so must be a save as.
            // options.setSaveInSubdirectory(true);// .... "" ...., so save into its own subdirectory.
            if (fileType == FileUtility.UNDEFINED) { // file type wasn't set, so call dialog
                fileType = getFileType(); // popup dialog to determine filetype

                if (unknownIODialog.isCancelled()) {
                    return;
                }

                suffix = unknownIODialog.getSuffix(); // get the expected suffix from the dialog

                options.setFileType(fileType);
            } else if (fileType == FileUtility.JIMI) { // if type is JIMI, then try and use suffix from fileInfo
                suffix = image.getFileInfo(0).getFileSuffix();

                // if suffix wasn't set, then use the default suffix for fileType
                if ( (suffix == null) || suffix.equals("")) {
                    suffix = FileTypeTable.getFileTypeInfo(fileType).getDefaultExtension();
                }

                Preferences.debug("FileIO save:  suffix = " + suffix + "\n", Preferences.DEBUG_FILEIO);
            } else {
                suffix = FileTypeTable.getFileTypeInfo(fileType).getDefaultExtension(); // get suffix from what the file
                // type should be
            }

            boolean append = true;
            index = (options.getFileName()).lastIndexOf('.');
            if (index > 0) {
                final String firstSuffix = (options.getFileName()).substring(index);
                if (firstSuffix.toUpperCase().equals(suffix.toUpperCase())) {
                    // Prevent generating a double suffix as in test.img.img
                    append = false;
                } else if ( (firstSuffix.toUpperCase().equals(".IMG")) && (suffix.toUpperCase().equals(".NII"))) {
                    // Prevent saving a 2 file nifti as a 1 file nifti by incorrectly changing
                    // test.img to test.img.nii
                    append = false;
                }
            }
            if (append) {
                options.setFileName(options.getFileName() + suffix); // append file extension
            }
        } else if (fileType == FileUtility.JIMI) { // if type is JIMI, then try and use suffix from fileInfo

            // if filename already has a suffix then don't need to do anything
            if (FileUtility.getExtension(options.getFileName()).equals("")) {
                suffix = image.getFileInfo(0).getFileSuffix();

                // if suffix wasn't set, then use the default suffix for fileType
                if ( (suffix == null) || suffix.equals("")) {
                    suffix = FileTypeTable.getFileTypeInfo(fileType).getDefaultExtension();
                }

                Preferences.debug("FileIO save:  suffix = " + suffix + "\n", Preferences.DEBUG_FILEIO);

                options.setFileName(options.getFileName() + suffix); // append file extension
            }
        }

        if (zip || gzip || bz2zip) {
            index = options.getFileName().lastIndexOf(".");

            if (index >= 0) {
                ext = options.getFileName().substring(index + 1);
                if (ext.equalsIgnoreCase("nii")) {
                    singleFileNIFTI = true;
                }
            }
            if ( ! (singleFileNIFTI || fileType == FileUtility.MINC || fileType == FileUtility.MINC_HDF || fileType == FileUtility.XML)) {

                MipavUtil.displayError("Compression only on single file nifti or minc or single file xml.");
                return;
            }
        }

        if ( !options.isSet()) {
            options.setFileType(fileType);
            options.setMultiFile(image.getFileInfo(0).getMultiFile());
            options.setPackBitEnabled( (fileType == FileUtility.TIFF)
                    && ( (image.getFileInfo(0).getDataType() == ModelStorageBase.BYTE) || (image.getFileInfo(0)
                            .getDataType() == ModelStorageBase.UBYTE)));
        }

        if (options.isSaveAs() && !options.isSet()) {
            // if(!gzip && !zip && !bz2zip) {
            if ( !callDialog(image.getExtents(), (fileType == FileUtility.TIFF), options)) {
                return;
            }
            if ( (gzip || zip || bz2zip) && options.isMultiFile()) {
                MipavUtil.displayError("Compression only on single file");
                return;
            }
            // }
        }

        boolean success = false;

        switch (fileType) {

            case FileUtility.RAW:
                success = writeRaw(image, options);
                break;

            case FileUtility.ANALYZE:
                success = writeAnalyze(image, options, zerofunused[0]);
                break;

            case FileUtility.NIFTI:
                if ( !gzip && !zip && !bz2zip) {
                    success = writeNIFTI(image, options);
                    break;
                } else {
                    break;
                }
            case FileUtility.MATLAB:
                success = writeMATLAB(image, options);
                break;
            case FileUtility.GE_SIGNA4X:
                success = writeGESigna4X(image, options);
                break;
            case FileUtility.GE_GENESIS:
                success = writeGEGenesis5X(image, options);
                break;
            case FileUtility.SPM:
                success = writeSPM(image, options);
                break;

            case FileUtility.TIFF:
                success = writeTiff(image, options);
                break;

            case FileUtility.STK:
                success = writeSTK(image, options);
                break;

            case FileUtility.MGH:
                success = writeMGH(image, options);
                break;

            case FileUtility.DICOM:
                // not handling 4d or greater images
                if (image.getNDims() > 3 && options.doEnhanced() != true) {
                    int enhancedError = JOptionPane.showConfirmDialog(null, "4D or greater image sets must be saved in enhanced dicom format, save as enhanced dicom?"
                                , image.getNDims()+"D image writing", JOptionPane.YES_NO_OPTION);
                    if(enhancedError == JOptionPane.NO_OPTION) {
                        break;
                    } else {
                        options.doEnhanced(true);
                    }
                    
                }

                // if we save off dicom as encapsulated jpeg2000, call the writeDicom method
                if (saveAsEncapJP2) {
                    success = writeDicom(image, options);
                    break;
                }

                // multiframe dicom images and 2D images
                if ( (image.isDicomImage() && ((FileInfoDicom) image.getFileInfo(0)).isMultiFrame())
                        || (image.getNDims() == 2)) {
                    success = writeDicom(image, options);
                    break;
                }

                // For 3D images, will call writeDicomSlice or writeDicom depending on how much memory is in use along
                // with how
                // big the image is and a factor since image cloning can occur
                final long memoryInUse = MipavUtil.getUsedHeapMemory();
                final long totalMemory = MipavUtil.getMaxHeapMemory();
                final int imageSize = image.getSize();
                int numBytes = 1;
                final int type = image.getType();
                ;
                if (type == ModelStorageBase.BOOLEAN || type == ModelStorageBase.BYTE || type == ModelStorageBase.UBYTE) {
                    numBytes = 1;
                } else if (type == ModelStorageBase.SHORT || type == ModelStorageBase.USHORT) {
                    numBytes = 2;
                } else if (type == ModelStorageBase.INTEGER || type == ModelStorageBase.UINTEGER
                        || type == ModelStorageBase.FLOAT || type == ModelStorageBase.ARGB) {
                    numBytes = 4;
                } else if (type == ModelStorageBase.LONG || type == ModelStorageBase.DOUBLE
                        || type == ModelStorageBase.ARGB_USHORT) {
                    numBytes = 8;
                } else if (type == ModelStorageBase.ARGB_FLOAT) {
                    numBytes = 16;
                }

                final long imageMemory = numBytes * imageSize;
                // factor used
                final float factor = 3f;
                // determine which write dicom method to call
                if ( (memoryInUse + (imageMemory * factor)) < (0.8 * totalMemory)) {
                    success = writeDicom(image, options);
                } else {
                    FileInfoDicom fileDicom = null;
                    if ( !image.isDicomImage()) {
                        fileDicom = new FileInfoDicom(options.getFileName(), fileDir, FileUtility.DICOM);

                        final JDialogSaveDicom dialog = new JDialogSaveDicom(UI.getMainFrame(), image.getFileInfo(0),
                                fileDicom, options.isScript());

                        if (dialog.isCancelled()) {
                            break;
                        }
                    }

                    createProgressBar(null, options.getFileName(), FileIO.FILE_WRITE);

                    final int beginSlice = options.getBeginSlice();
                    final int endSlice = options.getEndSlice();
                    final int sliceSize = image.getSliceSize();
                    int colorFactor = 1;
                    if (image.isColorImage()) {
                        colorFactor = 4;
                    }
                    final float[] imageBuffer = new float[colorFactor * sliceSize];
                    final int[] newExtents = new int[2];
                    newExtents[0] = image.getExtents()[0];
                    newExtents[1] = image.getExtents()[1];
                    ModelImage newImage;

                    // extract 2D slice from volume and write out
                    for (i = beginSlice; i <= endSlice; i++) {
                        try {
                            // extract 2D data to buffer
                            if (image.isColorImage()) {
                                image.exportData(i * 4 * sliceSize, 4 * sliceSize, imageBuffer);
                            } else {
                                image.exportData(i * sliceSize, sliceSize, imageBuffer);
                            }
                            // create new 2D ModelImage
                            newImage = new ModelImage(image.getType(), newExtents, image.getImageName() + "_slice");

                            // import data
                            newImage.importData(0, imageBuffer, false);

                            // set fileInfo
                            if (image.isDicomImage()) {
                                fileDicom = (FileInfoDicom) image.getFileInfo(i).clone();
                                fileDicom.setExtents(newExtents);
                                newImage.setFileInfo(fileDicom, 0);
                                newImage.calcMinMax();
                                // wrtie out DICOM slice
                                success = writeDicomSlice(newImage, i, options, null, null, -1, null, -1, -1, image
                                        .isDicomImage(), image.isColorImage());
                            } else {
                                fileDicom.setExtents(newExtents);
                                // newImage.setFileInfo(fileDicom, 0);
                                newImage.calcMinMax();
                                // wrtie out DICOM slice
                                success = writeDicomSlice(newImage, i, options, fileDicom, image.getFileInfo(0), image
                                        .getType(), image.getMatrix(), image.getMin(), image.getMax(), image
                                        .isDicomImage(), image.isColorImage());
                            }

                            // dispose 2D ModelImage
                            if (newImage != null) {
                                newImage.disposeLocal();
                                newImage = null;
                            }

                        } catch (final IOException e) {
                            e.printStackTrace();
                            break;
                        }
                    }
                }

                break;

            case FileUtility.JIMI:
                success = writeJimi(image, options);
                break;

            case FileUtility.MINC:
                success = writeMinc(image, options);
                break;

            case FileUtility.MINC_HDF:
                success = false;
                if ( !quiet) {
                    MipavUtil.displayError("Writing of Minc 2.0 files is currently unsupported.");
                }
                // TODO: MINC2 writing is disabled until we fix it to work with hdf-java-2.7
                // success = writeMincHDF(image, options);
                break;

            case FileUtility.INTERFILE:
                success = writeInterfile(image, options);
                break;

            case FileUtility.FITS:
                success = writeFits(image, options);
                break;

            case FileUtility.MRC:
                success = writeMRC(image, options);
                break;

            case FileUtility.COR:
                success = writeCOR(image, options);
                break;

            case FileUtility.AFNI:
                success = writeAfni(image, options);
                break;

            case FileUtility.ICS:
                success = writeICS(image, options);
                break;

            case FileUtility.XML:
                if (gzip || zip || bz2zip) {
                    options.setWriteHeaderOnly(true);
                    success = writeXML(image, options, false);
                } else {
                    success = writeXML(image, options, bDisplayProgress);
                }

                break;

            case FileUtility.AVI:
                success = writeAvi(image, options);
                break;
            case FileUtility.PARREC:
                success = writePARREC(image, options);
                break;

            case FileUtility.NRRD:
                success = writeNRRD(image, options);
                break;

            case FileUtility.JP2:
                success = writeJpeg2000(image, options);
                break;

            case FileUtility.VISTA:
                success = writeVista(image, options);
                break;

            default:
                if ( !quiet) {
                    MipavUtil
                            .displayError("File type unknown.  Try Save Image As; \notherwise, the file type is not supported.");
                }

                return;
        }

        // xml and nii zip, gzip,and bz2 files are written out without first saving off the uncompressed file to disk
        // and then
        // reading it in....rather it is streamed directly from the image buffer
        if (zip || gzip || bz2zip) {
            index = options.getFileName().lastIndexOf(".");

            if (index >= 0) {
                ext = options.getFileName().substring(index + 1);
                if (ext.equalsIgnoreCase("nii")) {
                    singleFileNIFTI = true;
                }
            }
            if (singleFileNIFTI || (fileType == FileUtility.MINC) || (fileType == FileUtility.MINC_HDF)
                    || (fileType == FileUtility.XML)) {
                if (zip) {
                    compressionExt = ".zip";
                } else if (gzip) {
                    compressionExt = ".gz";
                } else if (bz2zip) {
                    compressionExt = ".bz2";
                }
                createProgressBar(null, options.getFileName(), FileIO.FILE_WRITE);
                int progVal = 0;
                inputFileName = new String[1];
                outputFileName = new String[1];
                inputFileName[0] = options.getFileDirectory() + options.getFileName();
                numFiles = 1;

                if (fileType == FileUtility.XML) {
                    // For XML the user enters fileName.xml.gz or fileName.xml.bz2, but the xml header file is
                    // not compressed while the raw data file is compressed.
                    if (options.isMultiFile()) {
                        numFiles = dataFileName.length;
                        inputFileName = new String[numFiles];
                        outputFileName = new String[numFiles];
                        for (i = 0; i < numFiles; i++) {
                            inputFileName[i] = options.getFileDirectory() + dataFileName[i];
                        }
                    } else {
                        index = inputFileName[0].lastIndexOf(".");
                        inputFileName[0] = inputFileName[0].substring(0, index) + ".raw";
                    }
                }

                for (i = 0; i < numFiles; i++) {
                    outputFileName[i] = inputFileName[i] + compressionExt;

                    if (zip) {
                        progVal = progVal + (20 / numFiles);
                        progressBar.updateValue(progVal, false);
                        try {
                            // Create the ZIP output stream
                            zout = new ZipOutputStream(new FileOutputStream(outputFileName[i]));
                        } catch (final IOException e) {
                            MipavUtil.displayError("IOException on new ZipOutputStream");
                            return;
                        }

                        try {
                            zout.putNextEntry(new ZipEntry("data"));
                        } catch (final IOException e) {
                            MipavUtil.displayError("IOException on zout.putNextEntry");
                            return;
                        }

                        progVal = progVal + (20 / numFiles);
                        progressBar.updateValue(progVal, false);
                        if (singleFileNIFTI) {
                            // for single file nifti....we write out to gzout directly from image data
                            final FileNIFTI fileNIFTI = new FileNIFTI(options.getFileName(), options.getFileDirectory());
                            final int nImagesSaved = options.getEndSlice() - options.getBeginSlice() + 1;
                            final int nTimePeriodsSaved = options.getEndTime() - options.getBeginTime() + 1;
                            try {
                                final boolean oneFile = true;
                                fileNIFTI.writeHeader(image, nImagesSaved, nTimePeriodsSaved, options.getFileName(),
                                        options.getFileDirectory(), true, oneFile);
                            } catch (final IOException e) {
                                MipavUtil.displayError("IOException wrting header " + inputFileName[i]);
                                return;
                            }
                            final byte[] headerByteData = fileNIFTI.getBufferByte();
                            progVal = progVal + (20 / numFiles);
                            progressBar.updateValue(progVal, false);
                            // write out headerByteData
                            buf = new byte[1024];
                            for (int k = 0; k < headerByteData.length; k = k + 1024) {
                                int zipLen = 1024;
                                for (int b = 0, c = k; b < buf.length; b++, c++) {
                                    if (c < headerByteData.length) {
                                        buf[b] = headerByteData[c];
                                    } else {
                                        zipLen = b;
                                        break;
                                    }
                                }
                                try {
                                    zout.write(buf, 0, zipLen);
                                } catch (final IOException e) {
                                    MipavUtil.displayError("IOException on byte transfer to zip file");
                                    return;
                                }
                            }
                            int sliceLength = image.getExtents()[0] * image.getExtents()[1];
                            if (image.isColorImage()) {
                                sliceLength = sliceLength * 4;
                            }

                            byte[] sliceByteData;
                            progVal = progVal + (20 / numFiles);
                            progressBar.updateValue(progVal, false);
                            // write out image data

                            final int tS = options.getBeginTime();
                            final int tEnd = options.getEndTime();
                            final int s3d = options.getBeginSlice() * sliceLength;
                            final int e3d = (options.getEndSlice() + 1) * sliceLength;
                            int s4d = 0;
                            int s = 0;
                            int end = 0;
                            int e4d = 0;

                            for (int tStart = tS; tStart <= tEnd; tStart++) {
                                if (image.is4DImage()) {

                                    s4d = s3d + (tStart * image.getExtents()[2] * sliceLength);
                                    e4d = e3d + (tStart * image.getExtents()[2] * sliceLength);
                                    s = s4d;
                                    end = e4d;
                                } else {
                                    s = s3d;
                                    end = e3d;
                                }

                                for (int start = s; start < end; start = start + sliceLength) {
                                    sliceByteData = getByteImageData(image, start, sliceLength);
                                    for (int k = 0; k < sliceByteData.length; k = k + 1024) {
                                        int zipLen = 1024;
                                        for (int b = 0, c = k; b < buf.length; b++, c++) {
                                            if (c < sliceByteData.length) {
                                                buf[b] = sliceByteData[c];
                                            } else {
                                                zipLen = b;
                                                break;
                                            }
                                        }
                                        try {
                                            zout.write(buf, 0, zipLen);
                                        } catch (final Exception e) {
                                            e.printStackTrace();
                                            MipavUtil.displayError("IOException on byte transfer to zip file");
                                            return;
                                        }
                                    }
                                }

                            }

                            success = true;

                        } else if (fileType == FileUtility.XML) {
                            // header has already been writen out
                            buf = new byte[1024];
                            int sliceLength = image.getExtents()[0] * image.getExtents()[1];
                            if (image.isColorImage()) {
                                sliceLength = sliceLength * 4;
                            }

                            byte[] sliceByteData;
                            progVal = progVal + (20 / numFiles);
                            progressBar.updateValue(progVal, false);
                            // write out image data

                            final int tS = options.getBeginTime();
                            final int tEnd = options.getEndTime();
                            final int s3d = options.getBeginSlice() * sliceLength;
                            final int e3d = (options.getEndSlice() + 1) * sliceLength;
                            int s4d = 0;
                            int s = 0;
                            int end = 0;
                            int e4d = 0;

                            for (int tStart = tS; tStart <= tEnd; tStart++) {
                                if (image.is4DImage()) {

                                    s4d = s3d + (tStart * image.getExtents()[2] * sliceLength);
                                    e4d = e3d + (tStart * image.getExtents()[2] * sliceLength);
                                    s = s4d;
                                    end = e4d;
                                } else {
                                    s = s3d;
                                    end = e3d;
                                }

                                for (int start = s; start < end; start = start + sliceLength) {
                                    sliceByteData = getByteImageData(image, start, sliceLength);
                                    for (int k = 0; k < sliceByteData.length; k = k + 1024) {
                                        int zipLen = 1024;
                                        for (int b = 0, c = k; b < buf.length; b++, c++) {
                                            if (c < sliceByteData.length) {
                                                buf[b] = sliceByteData[c];
                                            } else {
                                                zipLen = b;
                                                break;
                                            }
                                        }
                                        try {
                                            zout.write(buf, 0, zipLen);
                                        } catch (final Exception e) {
                                            e.printStackTrace();
                                            MipavUtil.displayError("IOException on byte transfer to zip file");
                                            return;
                                        }
                                    }
                                }
                            }

                            success = true;

                        } else {
                            // Open the input file
                            try {
                                in = new FileInputStream(inputFileName[i]);
                            } catch (final IOException e) {
                                MipavUtil.displayError("IOException on new FileInputStream for " + inputFileName[i]);
                                return;
                            }
                            progVal = progVal + (20 / numFiles);
                            progressBar.updateValue(progVal, false);
                            // Tranfer the bytes from the input file to the Zip output stream
                            buf = new byte[1024];
                            try {
                                while ( (len = in.read(buf)) > 0) {
                                    zout.write(buf, 0, len);
                                }
                            } catch (final IOException e) {
                                MipavUtil.displayError("IOException on byte transfer to zip file");
                                return;
                            }
                            progVal = progVal + (20 / numFiles);
                            progressBar.updateValue(progVal, false);
                            try {
                                in.close();
                            } catch (final IOException e) {
                                MipavUtil.displayError("IOException on in.close()");
                                return;
                            }
                            inputFile = new File(inputFileName[i]);
                            // Delete the input file
                            try {
                                inputFile.delete();
                            } catch (final SecurityException sc) {
                                MipavUtil.displayError("Security error occurs while trying to delete "
                                        + inputFileName[i]);
                            }

                        }

                        // complete the zip file
                        try {
                            zout.finish();
                        } catch (final IOException e) {
                            MipavUtil.displayError("IOException on zout.finish()");
                            return;
                        }
                        progVal = progVal + (20 / numFiles);
                        progressBar.updateValue(progVal, false);
                        try {
                            zout.close();
                        } catch (final IOException e) {
                            MipavUtil.displayError("IOException on zout.close()");
                            return;
                        }
                    } // if (zip)
                    else if (gzip) {
                        progVal = progVal + (20 / numFiles);
                        progressBar.updateValue(progVal, false);
                        try {
                            // Create the GZIP output stream
                            gzout = new GZIPOutputStream(new FileOutputStream(outputFileName[i]));
                        } catch (final IOException e) {
                            MipavUtil.displayError("IOException on new GZIPOutputStream");
                            return;
                        }
                        progVal = progVal + (20 / numFiles);
                        progressBar.updateValue(progVal, false);
                        if (singleFileNIFTI) {
                            // for single file nifti....we write out to gzout directly from image data
                            final FileNIFTI fileNIFTI = new FileNIFTI(options.getFileName(), options.getFileDirectory());
                            final int nImagesSaved = options.getEndSlice() - options.getBeginSlice() + 1;
                            final int nTimePeriodsSaved = options.getEndTime() - options.getBeginTime() + 1;
                            try {
                                final boolean oneFile = true;
                                fileNIFTI.writeHeader(image, nImagesSaved, nTimePeriodsSaved, options.getFileName(),
                                        options.getFileDirectory(), true, oneFile);
                            } catch (final IOException e) {
                                MipavUtil.displayError("IOException wrting header " + inputFileName[i]);
                                return;
                            }
                            final byte[] headerByteData = fileNIFTI.getBufferByte();
                            progVal = progVal + (20 / numFiles);
                            progressBar.updateValue(progVal, false);
                            // write out headerByteData
                            buf = new byte[1024];
                            for (int k = 0; k < headerByteData.length; k = k + 1024) {
                                int gzipLen = 1024;
                                for (int b = 0, c = k; b < buf.length; b++, c++) {
                                    if (c < headerByteData.length) {
                                        buf[b] = headerByteData[c];
                                    } else {
                                        gzipLen = b;
                                        break;
                                    }
                                }
                                try {
                                    gzout.write(buf, 0, gzipLen);
                                } catch (final IOException e) {
                                    MipavUtil.displayError("IOException on byte transfer to gzip file");
                                    return;
                                }
                            }
                            int sliceLength = image.getExtents()[0] * image.getExtents()[1];
                            if (image.isColorImage()) {
                                sliceLength = sliceLength * 4;
                            }

                            byte[] sliceByteData;
                            progVal = progVal + (20 / numFiles);
                            progressBar.updateValue(progVal, false);
                            // write out image data

                            final int tS = options.getBeginTime();
                            final int tEnd = options.getEndTime();
                            final int s3d = options.getBeginSlice() * sliceLength;
                            final int e3d = (options.getEndSlice() + 1) * sliceLength;
                            int s4d = 0;
                            int s = 0;
                            int end = 0;
                            int e4d = 0;

                            for (int tStart = tS; tStart <= tEnd; tStart++) {
                                if (image.is4DImage()) {

                                    s4d = s3d + (tStart * image.getExtents()[2] * sliceLength);
                                    e4d = e3d + (tStart * image.getExtents()[2] * sliceLength);
                                    s = s4d;
                                    end = e4d;
                                } else {
                                    s = s3d;
                                    end = e3d;
                                }

                                for (int start = s; start < end; start = start + sliceLength) {
                                    sliceByteData = getByteImageData(image, start, sliceLength);
                                    for (int k = 0; k < sliceByteData.length; k = k + 1024) {
                                        int gzipLen = 1024;
                                        for (int b = 0, c = k; b < buf.length; b++, c++) {
                                            if (c < sliceByteData.length) {
                                                buf[b] = sliceByteData[c];
                                            } else {
                                                gzipLen = b;
                                                break;
                                            }
                                        }
                                        try {
                                            gzout.write(buf, 0, gzipLen);
                                        } catch (final Exception e) {
                                            e.printStackTrace();
                                            MipavUtil.displayError("IOException on byte transfer to gzip file");
                                            return;
                                        }
                                    }
                                }

                            }

                            success = true;
                        } else if (fileType == FileUtility.XML) {
                            buf = new byte[1024];
                            int sliceLength = image.getExtents()[0] * image.getExtents()[1];
                            if (image.isColorImage()) {
                                sliceLength = sliceLength * 4;
                            }

                            byte[] sliceByteData;
                            progVal = progVal + (20 / numFiles);
                            progressBar.updateValue(progVal, false);
                            // write out image data
                            final int tS = options.getBeginTime();
                            final int tEnd = options.getEndTime();
                            final int s3d = options.getBeginSlice() * sliceLength;
                            final int e3d = (options.getEndSlice() + 1) * sliceLength;
                            int s4d = 0;
                            int s = 0;
                            int end = 0;
                            int e4d = 0;

                            for (int tStart = tS; tStart <= tEnd; tStart++) {
                                if (image.is4DImage()) {

                                    s4d = s3d + (tStart * image.getExtents()[2] * sliceLength);
                                    e4d = e3d + (tStart * image.getExtents()[2] * sliceLength);
                                    s = s4d;
                                    end = e4d;
                                } else {
                                    s = s3d;
                                    end = e3d;
                                }

                                for (int start = s; start < end; start = start + sliceLength) {
                                    sliceByteData = getByteImageData(image, start, sliceLength);
                                    for (int k = 0; k < sliceByteData.length; k = k + 1024) {
                                        int gzipLen = 1024;
                                        for (int b = 0, c = k; b < buf.length; b++, c++) {
                                            if (c < sliceByteData.length) {
                                                buf[b] = sliceByteData[c];
                                            } else {
                                                gzipLen = b;
                                                break;
                                            }
                                        }
                                        try {
                                            gzout.write(buf, 0, gzipLen);
                                        } catch (final Exception e) {
                                            e.printStackTrace();
                                            MipavUtil.displayError("IOException on byte transfer to gzip file");
                                            return;
                                        }
                                    }
                                }

                            }

                            success = true;
                        }

                        else {
                            // Open the input file
                            try {
                                in = new FileInputStream(inputFileName[i]);

                            } catch (final IOException e) {
                                MipavUtil.displayError("IOException on new FileInputStream for " + inputFileName[i]);
                                return;
                            }
                            progVal = progVal + (20 / numFiles);
                            progressBar.updateValue(progVal, false);
                            // Tranfer the bytes from the input file to the GZIP output stream
                            buf = new byte[1024];
                            try {
                                while ( (len = in.read(buf)) > 0) {
                                    gzout.write(buf, 0, len);
                                }
                            } catch (final IOException e) {
                                MipavUtil.displayError("IOException on byte transfer to gzip file");
                                return;
                            }
                            progVal = progVal + (20 / numFiles);
                            progressBar.updateValue(progVal, false);
                            try {
                                in.close();
                            } catch (final IOException e) {
                                MipavUtil.displayError("IOException on in.close()");
                                return;
                            }
                            inputFile = new File(inputFileName[i]);
                            // Delete the input file
                            try {
                                inputFile.delete();
                            } catch (final SecurityException sc) {
                                MipavUtil.displayError("Security error occurs while trying to delete "
                                        + inputFileName[i]);
                            }
                        }

                        // complete the gzip file
                        try {
                            gzout.finish();
                        } catch (final IOException e) {
                            MipavUtil.displayError("IOException on gzout.finish()");
                            return;
                        }
                        progVal = progVal + (20 / numFiles);
                        progressBar.updateValue(progVal, false);
                        try {
                            gzout.close();
                        } catch (final IOException e) {
                            MipavUtil.displayError("IOException on gzout.close()");
                            return;
                        }
                    } // if (gzip)
                    else { // bz2zip
                        progVal = progVal + (20 / numFiles);
                        progressBar.updateValue(progVal, false);
                        try {
                            out = new FileOutputStream(outputFileName[i]);
                            out.write('B');
                            out.write('Z');
                            // Create the BZIP2 output stream
                            bz2out = new CBZip2OutputStream(out);
                        } catch (final IOException e) {
                            MipavUtil.displayError("IOException on new CBZip2OutputStream");
                            return;
                        }
                        progVal = progVal + (20 / numFiles);
                        progressBar.updateValue(progVal, false);
                        if (singleFileNIFTI) {
                            // for single file nifti....we write out to gzout directly from image data
                            final FileNIFTI fileNIFTI = new FileNIFTI(options.getFileName(), options.getFileDirectory());
                            final int nImagesSaved = options.getEndSlice() - options.getBeginSlice() + 1;
                            final int nTimePeriodsSaved = options.getEndTime() - options.getBeginTime() + 1;
                            try {
                                final boolean oneFile = true;
                                fileNIFTI.writeHeader(image, nImagesSaved, nTimePeriodsSaved, options.getFileName(),
                                        options.getFileDirectory(), true, oneFile);
                            } catch (final IOException e) {
                                MipavUtil.displayError("IOException wrting header " + inputFileName[i]);
                                return;
                            }
                            final byte[] headerByteData = fileNIFTI.getBufferByte();
                            progVal = progVal + (20 / numFiles);
                            progressBar.updateValue(progVal, false);
                            // write out headerByteData
                            buf = new byte[1024];
                            for (int k = 0; k < headerByteData.length; k = k + 1024) {
                                int bzipLen = 1024;
                                for (int b = 0, c = k; b < buf.length; b++, c++) {
                                    if (c < headerByteData.length) {
                                        buf[b] = headerByteData[c];
                                    } else {
                                        bzipLen = b;
                                        break;
                                    }
                                }
                                try {
                                    bz2out.write(buf, 0, bzipLen);
                                } catch (final IOException e) {
                                    MipavUtil.displayError("IOException on byte transfer to bz2zip file");
                                    return;
                                }
                            }
                            int sliceLength = image.getExtents()[0] * image.getExtents()[1];
                            if (image.isColorImage()) {
                                sliceLength = sliceLength * 4;
                            }

                            byte[] sliceByteData;
                            progVal = progVal + (20 / numFiles);
                            progressBar.updateValue(progVal, false);
                            // write out image data

                            final int tS = options.getBeginTime();
                            final int tEnd = options.getEndTime();
                            final int s3d = options.getBeginSlice() * sliceLength;
                            final int e3d = (options.getEndSlice() + 1) * sliceLength;
                            int s4d = 0;
                            int s = 0;
                            int end = 0;
                            int e4d = 0;

                            for (int tStart = tS; tStart <= tEnd; tStart++) {
                                if (image.is4DImage()) {

                                    s4d = s3d + (tStart * image.getExtents()[2] * sliceLength);
                                    e4d = e3d + (tStart * image.getExtents()[2] * sliceLength);
                                    s = s4d;
                                    end = e4d;
                                } else {
                                    s = s3d;
                                    end = e3d;
                                }

                                for (int start = s; start < end; start = start + sliceLength) {
                                    sliceByteData = getByteImageData(image, start, sliceLength);
                                    for (int k = 0; k < sliceByteData.length; k = k + 1024) {
                                        int bzipLen = 1024;
                                        for (int b = 0, c = k; b < buf.length; b++, c++) {
                                            if (c < sliceByteData.length) {
                                                buf[b] = sliceByteData[c];
                                            } else {
                                                bzipLen = b;
                                                break;
                                            }
                                        }
                                        try {
                                            bz2out.write(buf, 0, bzipLen);
                                        } catch (final Exception e) {
                                            e.printStackTrace();
                                            MipavUtil.displayError("IOException on byte transfer to bz2zip file");
                                            return;
                                        }
                                    }
                                }
                            }

                            success = true;
                        } else if (fileType == FileUtility.XML) {
                            buf = new byte[1024];
                            int sliceLength = image.getExtents()[0] * image.getExtents()[1];
                            if (image.isColorImage()) {
                                sliceLength = sliceLength * 4;
                            }

                            byte[] sliceByteData;
                            progVal = progVal + (20 / numFiles);
                            progressBar.updateValue(progVal, false);
                            // write out image data
                            final int tS = options.getBeginTime();
                            final int tEnd = options.getEndTime();
                            final int s3d = options.getBeginSlice() * sliceLength;
                            final int e3d = (options.getEndSlice() + 1) * sliceLength;
                            int s4d = 0;
                            int s = 0;
                            int end = 0;
                            int e4d = 0;

                            for (int tStart = tS; tStart <= tEnd; tStart++) {
                                if (image.is4DImage()) {

                                    s4d = s3d + (tStart * image.getExtents()[2] * sliceLength);
                                    e4d = e3d + (tStart * image.getExtents()[2] * sliceLength);
                                    s = s4d;
                                    end = e4d;
                                } else {
                                    s = s3d;
                                    end = e3d;
                                }

                                for (int start = s; start < end; start = start + sliceLength) {
                                    sliceByteData = getByteImageData(image, start, sliceLength);
                                    for (int k = 0; k < sliceByteData.length; k = k + 1024) {
                                        int bzipLen = 1024;
                                        for (int b = 0, c = k; b < buf.length; b++, c++) {
                                            if (c < sliceByteData.length) {
                                                buf[b] = sliceByteData[c];
                                            } else {
                                                bzipLen = b;
                                                break;
                                            }
                                        }
                                        try {
                                            bz2out.write(buf, 0, bzipLen);
                                        } catch (final Exception e) {
                                            e.printStackTrace();
                                            MipavUtil.displayError("IOException on byte transfer to bz2zip file");
                                            return;
                                        }
                                    }
                                }
                            }

                            success = true;
                        } else {
                            // Open the input file
                            try {
                                in = new FileInputStream(inputFileName[i]);
                            } catch (final IOException e) {
                                MipavUtil.displayError("IOException on new FileInputStream for " + inputFileName[i]);
                                return;
                            }
                            progVal = progVal + (20 / numFiles);
                            progressBar.updateValue(progVal, false);
                            // Tranfer the bytes from the input file to the BZIP2 output stream
                            buf = new byte[1024];
                            try {
                                while ( (len = in.read(buf)) > 0) {
                                    bz2out.write(buf, 0, len);
                                }
                            } catch (final IOException e) {
                                MipavUtil.displayError("IOException on byte transfer to bz2zip file");
                                return;
                            }
                            progVal = progVal + (20 / numFiles);
                            progressBar.updateValue(progVal, false);
                            try {
                                in.close();
                            } catch (final IOException e) {
                                MipavUtil.displayError("IOException on in.close()");
                                return;
                            }
                            inputFile = new File(inputFileName[i]);
                            // Delete the input file
                            try {
                                inputFile.delete();
                            } catch (final SecurityException sc) {
                                MipavUtil.displayError("Security error occurs while trying to delete "
                                        + inputFileName[i]);
                            }
                        }

                        progVal = progVal + (20 / numFiles);
                        progressBar.updateValue(progVal, false);
                        // complete the bz2zip file
                        try {
                            bz2out.close();
                        } catch (final IOException e) {
                            MipavUtil.displayError("IOException on bz2out.close()");
                            return;
                        }
                    } // else bz2zip
                } // for (i = 0; i < numFiles; i++)

            } // if (singleFileNIFTI || (fileType == FileUtility.MINC) || (fileType == FileUtility.MINC_HDF) ||
            else {
                MipavUtil.displayError("Compression only on single file nifti or minc or single file xml");
            }
        } // if (zip || gzip || bz2zip)

        if (progressBar != null) {
            progressBar.dispose();
        }

        // now checks to make sure we're not writing NDAR srb transfers (xml header only)
        if (success && ProvenanceRecorder.getReference().getRecorderStatus() == ProvenanceRecorder.RECORDING
                && !options.writeHeaderOnly()) {

            ScriptableActionInterface action;

            if (options.isSaveAs()) {
                action = new ActionSaveImageAs(image, options);
            } else {
                action = new ActionSaveImage(image, options);
            }

            ProvenanceRecorder.getReference().addLine(action);

            if (Preferences.is(Preferences.PREF_IMAGE_LEVEL_DATA_PROVENANCE)) {
                FileDataProvenance fdp;
                if (options.getFileName().lastIndexOf(".") != -1) {
                    fdp = new FileDataProvenance(options.getFileName().substring(0,
                            options.getFileName().lastIndexOf("."))
                            + ".xmp", options.getFileDirectory(), image.getProvenanceHolder());
                } else {
                    fdp = new FileDataProvenance(options.getFileName() + ".xmp", options.getFileDirectory(), image
                            .getProvenanceHolder());
                }
                try {
                    fdp.writeXML();
                } catch (final Exception e) {}
            }
        }

        // if the flag is set to put the image into the quicklist, do so
        if (success && options.doPutInQuicklist()) {

            if (options.isMultiFile()) {
                String fName = options.getFileName();
                String start = Integer.toString(options.getStartNumber());
                final int numDig = options.getDigitNumber();

                for (i = 1; i < numDig; i++) {
                    start = "0" + start;
                }

                if (fName.indexOf(".") != -1) {
                    fName = fName.substring(0, fName.indexOf(".")) + start
                            + fName.substring(fName.indexOf("."), fName.length());
                } else {
                    fName = fName + start;
                }

                // check to see if we are actually switching dims (split into multi-file)
                if (options.getBeginSlice() == options.getEndSlice()) {
                    if (image.getNDims() == 3) {
                        Preferences.setLastImage(options.getFileDirectory() + fName, false, 2);
                    } else {
                        // image nDims is 4
                        if (options.getBeginTime() == options.getEndTime()) {
                            Preferences.setLastImage(options.getFileDirectory() + fName, false, 2);
                        } else {
                            Preferences.setLastImage(options.getFileDirectory() + fName, false, 3);
                        }
                    }
                } else {
                    Preferences.setLastImage(options.getFileDirectory() + fName, true, image.getNDims());
                }
            } else {
                // single file format
                Preferences.setLastImage(options.getFileDirectory() + options.getFileName(), false, image.getNDims());
            }

            // updates menubar for each image when in interactive mode
            if(!quiet) {
	            if (UI.getImageFrameVector().size() < 1) {
	                UI.buildMenu();
	                UI.setControls();
	            } else {
	                UI.buildMenu();
	
	                for (i = 0; i < UI.getImageFrameVector().size(); i++) {
	
	                    if (UI.getImageFrameVector().elementAt(i) instanceof ViewJFrameImage) {
	                        ((ViewJFrameImage) (UI.getImageFrameVector().elementAt(i))).updateMenubar();
	                    }
	                }
	
	                UI.getActiveImageFrame().setControls();
	            }
            }
        }

        if (success) {
            ScriptableActionInterface action;

            if (options.isSaveAs()) {
                action = new ActionSaveImageAs(image, options);
            } else {
                action = new ActionSaveImage(image, options);
            }

            ScriptRecorder.getReference().addLine(action);
        }
    }

    public byte[] getByteImageData(final ModelImage image, final int start, final int length) {
        byte[] byteData = null;
        int type;

        final boolean bigEndianness = image.getFileInfo(0).getEndianess();
        type = image.getType();

        if (type == ModelStorageBase.BYTE || type == ModelStorageBase.UBYTE || type == ModelStorageBase.BOOLEAN) {
            byteData = new byte[length];
            try {
                image.exportData(start, length, byteData);
            } catch (final IOException error) {
                System.out.println("IO exception");
                // setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
                return null;
            }
        } else if (type == ModelStorageBase.SHORT || type == ModelStorageBase.USHORT) {
            final short[] shortData = new short[length];
            try {
                image.exportData(start, length, shortData);
            } catch (final IOException error) {
                System.out.println("IO exception");
                // setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
                return null;
            }
            byteData = new byte[length * 2];
            final byte[] buff = new byte[2];
            for (int i = 0, k = 0; i < shortData.length; i++, k = k + 2) {
                final short val = shortData[i];
                final byte[] byteVal = FileBase.shortToBytes(val, bigEndianness, buff);
                byteData[k] = byteVal[0];
                byteData[k + 1] = byteVal[1];
            }
        } else if (type == ModelStorageBase.INTEGER || type == ModelStorageBase.UINTEGER) {
            final int[] intData = new int[length];
            try {
                image.exportData(start, length, intData);
            } catch (final IOException error) {
                System.out.println("IO exception");
                // setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
                return null;
            }
            byteData = new byte[length * 4];
            final byte[] buff = new byte[4];
            for (int i = 0, k = 0; i < intData.length; i++, k = k + 4) {
                final int val = intData[i];
                final byte[] byteVal = FileBase.intToBytes(val, bigEndianness, buff);
                byteData[k] = byteVal[0];
                byteData[k + 1] = byteVal[1];
                byteData[k + 2] = byteVal[2];
                byteData[k + 3] = byteVal[3];
            }
        } else if (type == ModelStorageBase.LONG) {
            final long[] longData = new long[length];
            try {
                image.exportData(start, length, longData);
            } catch (final IOException error) {
                System.out.println("IO exception");
                // setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
                return null;
            }
            byteData = new byte[length * 8];
            final byte[] buff = new byte[8];
            for (int i = 0, k = 0; i < longData.length; i++, k = k + 8) {
                final long val = longData[i];
                final byte[] byteVal = FileBase.longToBytes(val, bigEndianness, buff);
                byteData[k] = byteVal[0];
                byteData[k + 1] = byteVal[1];
                byteData[k + 2] = byteVal[2];
                byteData[k + 3] = byteVal[3];
                byteData[k + 4] = byteVal[4];
                byteData[k + 5] = byteVal[5];
                byteData[k + 6] = byteVal[6];
                byteData[k + 7] = byteVal[7];
            }
        } else if (type == ModelStorageBase.FLOAT) {
            final float[] floatData = new float[length];
            try {
                image.exportData(start, length, floatData);
            } catch (final IOException error) {
                System.out.println("IO exception");
                // setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
                return null;
            }
            byteData = new byte[length * 4];
            final byte[] buff = new byte[4];
            for (int i = 0, k = 0; i < floatData.length; i++, k = k + 4) {
                final float val = floatData[i];
                final byte[] byteVal = FileBase.floatToBytes(val, bigEndianness, buff);
                byteData[k] = byteVal[0];
                byteData[k + 1] = byteVal[1];
                byteData[k + 2] = byteVal[2];
                byteData[k + 3] = byteVal[3];
            }
        } else if (type == ModelStorageBase.DOUBLE) {
            final double[] doubleData = new double[length];
            try {
                image.exportData(start, length, doubleData);
            } catch (final IOException error) {
                System.out.println("IO exception");
                // setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
                return null;
            }
            byteData = new byte[length * 8];
            final byte[] buff = new byte[8];
            for (int i = 0, k = 0; i < doubleData.length; i++, k = k + 8) {
                final double val = doubleData[i];
                final byte[] byteVal = FileBase.doubleToBytes(val, bigEndianness, buff);
                byteData[k] = byteVal[0];
                byteData[k + 1] = byteVal[1];
                byteData[k + 2] = byteVal[2];
                byteData[k + 3] = byteVal[3];
                byteData[k + 4] = byteVal[4];
                byteData[k + 5] = byteVal[5];
                byteData[k + 6] = byteVal[6];
                byteData[k + 7] = byteVal[7];
            }
        } else if (type == ModelStorageBase.ARGB) {
            byteData = new byte[length];
            try {
                image.exportData(start, length, byteData);
            } catch (final IOException error) {
                System.out.println("IO exception");
                // setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
                return null;
            }
            final byte[] byteData2 = new byte[byteData.length - byteData.length / 4];
            int counter = 0;
            for (int i = 0; i < byteData.length; i = i + 4) {
                byteData2[counter] = byteData[i + 1];
                byteData2[counter + 1] = byteData[i + 2];
                byteData2[counter + 2] = byteData[i + 3];
                counter = counter + 3;
            }
            return byteData2;
        } else if (type == ModelStorageBase.ARGB_USHORT) {
            final short[] shortData = new short[length];
            try {
                image.exportData(start, length, shortData);
            } catch (final IOException error) {
                System.out.println("IO exception");
                // setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
                return null;
            }
            byteData = new byte[length * 2];
            final byte[] buff = new byte[2];
            for (int i = 0, k = 0; i < shortData.length; i++, k = k + 2) {
                final short val = shortData[i];
                final byte[] byteVal = FileBase.shortToBytes(val, bigEndianness, buff);
                byteData[k] = byteVal[0];
                byteData[k + 1] = byteVal[1];
            }

            final byte[] byteData2 = new byte[byteData.length - byteData.length / 4];
            int counter = 0;
            for (int i = 0; i < byteData.length; i = i + 4) {
                byteData2[counter] = byteData[i + 1];
                byteData2[counter + 1] = byteData[i + 2];
                byteData2[counter + 2] = byteData[i + 3];
                counter = counter + 3;
            }
            return byteData2;

        } else if (type == ModelStorageBase.ARGB_FLOAT) {
            final float[] floatData = new float[length];
            try {
                image.exportData(start, length, floatData);
            } catch (final IOException error) {
                System.out.println("IO exception");
                // setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
                return null;
            }
            byteData = new byte[length * 4];
            final byte[] buff = new byte[4];
            for (int i = 0, k = 0; i < floatData.length; i++, k = k + 4) {
                final float val = floatData[i];
                final byte[] byteVal = FileBase.floatToBytes(val, bigEndianness, buff);
                byteData[k] = byteVal[0];
                byteData[k + 1] = byteVal[1];
                byteData[k + 2] = byteVal[2];
                byteData[k + 3] = byteVal[3];
            }

            final byte[] byteData2 = new byte[byteData.length - byteData.length / 4];
            int counter = 0;
            for (int i = 0; i < byteData.length; i = i + 4) {
                byteData2[counter] = byteData[i + 1];
                byteData2[counter + 1] = byteData[i + 2];
                byteData2[counter + 2] = byteData[i + 3];
                counter = counter + 3;
            }
            return byteData2;
        }

        return byteData;
    }

    /**
     * Provides a method of conversion from <code>FileInfoDicom</CODE> to <CODE>FileInfoImageXML</CODE>, by filling
     * the <CODE>FileInfoImageXML</CODE> with sets of chosen image information (from the DICOM tags).
     * 
     * <p>
     * The XML format suggests that each DICOM tag become a seperate set, and any multiple values stored in the DICOM
     * tag, becomes parameter data. The set name is the DICOM tag name. Each value of the tag is stored as a separate
     * parameter in the set, and its description is stored as the DICOM key (group and element number, as displayed in
     * the file, &quot;dicom.dictionary&quot;) along with its position in the value multiplicity. All values stored are
     * stored as value-type &quot;string&quot;, and neither date nor time are set. Order of entries is not guaranteed,
     * as order is not meaningful in XML.
     * </p>
     * 
     * <p>
     * Be sure to see the image.xsd file for more information.
     * </p>
     * 
     * @see #getDicomSaveList(FileInfoDicom)
     * @see JDialogDicom2XMLSelection
     * 
     * @param sourceInfo The FileInfoDicom which is the source for user-selectable tags
     * @param destInfo The FileInfoBase that holds the image information to be stored in XML (or MincHDF)format.
     */
    protected boolean dataConversion(final FileInfoBase sourceInfo, final FileInfoBase destInfo) {

        // when the original image is a DICOM or MINCimage, we want to save this as
        // XML, so look, or ask, for a dicom dictionary list of tags to save
        // into the XML
        // load the tags to keep:

        Hashtable<FileDicomKey, FileDicomTag> tags2save = null;
        if (sourceInfo instanceof FileInfoDicom) {
            tags2save = getDicomSaveList((FileInfoDicom) sourceInfo, destInfo instanceof FileInfoImageXML);
        } else if (sourceInfo instanceof FileInfoMincHDF) {
            final Hashtable<String, String> mincTags = ((FileInfoMincHDF) sourceInfo).getDicomTable();
            final Enumeration<String> e = mincTags.keys();
            tags2save = new Hashtable<FileDicomKey, FileDicomTag>(mincTags.size());
            while (e.hasMoreElements()) {
                final String keyStr = e.nextElement();
                final FileDicomKey key = new FileDicomKey(keyStr);
                if (DicomDictionary.getInfo(key) != null) {
                    final FileDicomTag tag = new FileDicomTag(DicomDictionary.getInfo(key));
                    try {
                        tag.setValue(mincTags.get(keyStr));
                        tags2save.put(key, tag);
                    } catch (final Exception ex) {
                        Preferences.debug("Error in setting value: " + mincTags.get(keyStr) + " of tag: " + keyStr
                                + "\n", Preferences.DEBUG_FILEIO);
                    }
                } else {
                    Preferences.debug("Tag not in Dicom Dictionary..skipping over tag: " + keyStr + "\n",
                            Preferences.DEBUG_FILEIO);
                }

            }
        }

        if (tags2save == null) {
            return false;
        }

        if (destInfo instanceof FileInfoImageXML) {
            final FileInfoImageXML dInfo = (FileInfoImageXML) destInfo;

            // now convert that DICOM tags list into an XML tags List:
            final Enumeration<FileDicomKey> e = tags2save.keys();
            while (e.hasMoreElements()) {
                final FileDicomKey tagKey = e.nextElement();
                final FileDicomTag dicomTag = tags2save.get(tagKey);

                dInfo.createPSet(dicomTag.getName());

                Object[] tagValues = new Object[0];

                try {
                    tagValues = dicomTag.getValueList();
                } catch (final NullPointerException npe) {
                    tagValues[0] = null;
                }

                // set the parameter values for as many values as the tag holds:
                for (int q = 0; q < tagValues.length; q++) {

                    if (tagValues[0] == null) {
                        continue;
                    }

                    // write the DICOM tags & their values into the XML file.
                    dInfo.getCurrentPSet().addParameter(dicomTag.getName() + "[" + q + "]");
                    dInfo.getCurrentPSet().getCurrentParameter().setDescription("(" + tagKey + ") [" + q + "]");
                    dInfo.getCurrentPSet().getCurrentParameter().setValueType("string");

                    try {

                        if (tagValues[q] == null || tagValues[q].toString().indexOf(0) != -1) {

                            // if there are NULL characters, remove them
                            // before saving the value
                            StringBuffer noNulls;
                            try {
                                noNulls = new StringBuffer(tagValues[q].toString());
                            } catch (final NullPointerException e1) {
                                noNulls = new StringBuffer("");
                            }

                            try {

                                while (noNulls.indexOf("\u0000") != -1) { // removing NULL
                                    noNulls.deleteCharAt(noNulls.indexOf("\u0000"));
                                }

                                dInfo.getCurrentPSet().getCurrentParameter().setValue(noNulls.toString());
                            } catch (final StringIndexOutOfBoundsException nullNotThere) {
                                dInfo.getCurrentPSet().getCurrentParameter().setValue("");
                                System.err.println("(" + tagKey + ") Trying to output " + "current string bounded by "
                                        + "colons, nulls and all:");

                                try {
                                    System.err.println(":" + noNulls.toString() + ":");
                                } catch (final Exception couldnt) {
                                    System.err.println("...Couldn't output noNulls string.");
                                }

                                Preferences.debug("Error converting DICOM to XML.", Preferences.DEBUG_FILEIO);
                                Preferences.debug("  Empty string written for :", Preferences.DEBUG_FILEIO);
                                Preferences.debug(" (" + tagKey + ")\n", Preferences.DEBUG_FILEIO);
                            }
                        } else {
                            dInfo.getCurrentPSet().getCurrentParameter().setValue(tagValues[q].toString());
                        }
                    } catch (final NullPointerException npe) {
                        Preferences.debug("Error converting DICOM to XML.", Preferences.DEBUG_FILEIO);
                        Preferences.debug("  Empty string written for:", Preferences.DEBUG_FILEIO);
                        Preferences.debug(" (" + tagKey + ")\n", Preferences.DEBUG_FILEIO);
                        dInfo.getCurrentPSet().getCurrentParameter().setValue("");
                    }
                }
            }
        }

        return true;
    }

    /**
     * Returns a list of tags that are a subset of the DICOM dictionary. This method chooses the tags to be saved by
     * presenting a dialog for the user to select a list of tags or by reading the
     * <q>dicomsave.dictionary</q>
     * file.
     * 
     * @param sourceInfo Source of DICOM information.
     * 
     * @return The Hashtable filled as an XML
     */
    protected Hashtable<FileDicomKey, FileDicomTag> getDicomSaveList(final FileInfoDicom sourceInfo, final boolean isXML) {
        Hashtable<FileDicomKey, FileDicomTagInfo> tags2save;

        if (quiet) { // don't bother asking user when running a macro.
            tags2save = DicomDictionary.getSubsetDicomTagTable();

            if (tags2save == null) {
                System.out.println("tags2save is null");
            }
        } else {
            final JDialogDicom2XMLSelection jdl = new JDialogDicom2XMLSelection(sourceInfo, isXML);
            jdl.setVisible(true);

            if (jdl.wasOkay()) {
                tags2save = jdl.getSaveTable(); // hack!!
            } else {
                return null;
            }
        }

        if (tags2save == null) {
            tags2save = new Hashtable<FileDicomKey, FileDicomTagInfo>();
        }

        final Hashtable<FileDicomKey, FileDicomTag> fullTagsList = sourceInfo.getTagTable().getTagList();
        final Hashtable<FileDicomKey, FileDicomTag> tagsWithValues = new Hashtable<FileDicomKey, FileDicomTag>(Math
                .min(tags2save.size(), fullTagsList.size()));

        // place DICOM tags (with values) into the save-tags list.
        // Remove any tags from the list that do not have values:
        final Enumeration<FileDicomKey> e = tags2save.keys();

        while (e.hasMoreElements()) {
            final FileDicomKey tagKey = e.nextElement();

            if (fullTagsList.containsKey(tagKey) && (fullTagsList.get(tagKey).getValue(false) != null)
                    && (fullTagsList.get(tagKey).getNumberOfValues() > 0)) {
                tags2save.put(tagKey, fullTagsList.get(tagKey).getInfo());
                tagsWithValues.put(tagKey, fullTagsList.get(tagKey));
            } else {
                tags2save.remove(tagKey);
            }
        }

        return tagsWithValues;
    }

    /**
     * Provides a method of conversion from <code>FileInfoLSM</CODE> to <CODE>FileInfoImageXML</CODE>, by filling
     * the <CODE>FileInfoImageXML</CODE> with sets of chosen image information.
     * 
     * @param sourceInfo the LSM-formatted Source information
     * @param destInfo the XML-format file information that is the output.
     */
    protected void LSMDataConversion(final FileInfoLSM sourceInfo, final FileInfoImageXML destInfo) {
        int firstSliceAfterBleach;
        int bleachedROIShape;
        String shapeString = null;
        double[] knotX = null;
        double[] timeStamp = null;
        int q;

        firstSliceAfterBleach = sourceInfo.getFirstSliceAfterBleach();

        if (firstSliceAfterBleach >= 0) {
            destInfo.createPSet("firstSliceAfterBleach");
            destInfo.getCurrentPSet().addParameter("firstSliceAfterBleach");
            destInfo.getCurrentPSet().getCurrentParameter().setValueType("int");
            destInfo.getCurrentPSet().getCurrentParameter().setDescription("Zero based");
            destInfo.getCurrentPSet().getCurrentParameter().setValue(String.valueOf(firstSliceAfterBleach));
        }

        bleachedROIShape = sourceInfo.getBleachedROIShape();

        if (bleachedROIShape >= 0) {
            destInfo.createPSet("bleachedROIShape");
            destInfo.getCurrentPSet().addParameter("bleachedROIShape");
            destInfo.getCurrentPSet().getCurrentParameter().setValueType("int");

            switch (bleachedROIShape) {

                case 18:
                    shapeString = "Rectangle";
                    break;

                case 19:
                    shapeString = "Ellipse";
                    break;

                case 20:
                    shapeString = "Closed polyline";
                    break;

                case 22:
                    shapeString = "Closed bezier";
                    break;

                case 24:
                    shapeString = "Circle";
                    break;
            }

            if (shapeString != null) {
                destInfo.getCurrentPSet().getCurrentParameter().setDescription(shapeString);
            }

            destInfo.getCurrentPSet().getCurrentParameter().setValue(String.valueOf(bleachedROIShape));
        }

        knotX = sourceInfo.getKnotX();

        if (knotX != null) {
            destInfo.createPSet("knotX");

            for (q = 0; q < knotX.length; q++) {
                destInfo.getCurrentPSet().addParameter("knotX[" + q + "]");
                destInfo.getCurrentPSet().getCurrentParameter().setValueType("double");
                destInfo.getCurrentPSet().getCurrentParameter().setValue(String.valueOf(knotX[q]));
            }
        }

        timeStamp = sourceInfo.getTimeStamp();

        if (timeStamp != null) {
            destInfo.createPSet("timeStamp");

            for (q = 0; q < timeStamp.length; q++) {
                destInfo.getCurrentPSet().addParameter("timeStamp[" + q + "]");
                destInfo.getCurrentPSet().getCurrentParameter().setValueType("double");
                destInfo.getCurrentPSet().getCurrentParameter().setValue(String.valueOf(timeStamp[q]));
            }
        }

    }

    /**
     * DOCUMENT ME!
     * 
     * @param modelImageSrc DOCUMENT ME!
     * @param modelImageResult DOCUMENT ME!
     * @param sliceNum DOCUMENT ME!
     */
    private static void copyResolutions(final ModelImage modelImageSrc, final ModelImage modelImageResult,
            final int sliceNum) {
        final float[] resolutions = new float[3];
        resolutions[0] = modelImageSrc.getFileInfo(0).getResolution(0);
        resolutions[1] = modelImageSrc.getFileInfo(0).getResolution(1);
        resolutions[2] = 1.0f;

        modelImageResult.getFileInfo(sliceNum).setResolutions(resolutions);
    }

    /**
     * The purpose of this method is to take a buffer of float and resample the values to proper UBYTE values.
     * 
     * @param buffer float[] - the buffer to be sampled
     * @param min - the image's minimum pixel value
     * @param max - the image's maximum pixel value
     * 
     * @return float[] - the resampled buffer with min and max values in the range 0 - 255
     */
    private static float[] resample255(final float[] buffer, final float min, final float max) {
        final float precalculatedDenominator = max - min; // precalculated (max - min) for speed

        for (int i = 0; i < buffer.length; i++) {
            buffer[i] = ( (buffer[i] - min) / precalculatedDenominator) * 255;
        }

        return buffer;
    }

    /**
     * Sorts an array of floats (using Arrays.sort), turns into array of ints used to sort images by A;
     * thus, the final value of B[i] is for the ith image read in, put that image at location B[i] in the image buffer.
     * This is necessary for images labeled dicom1, dicom2, etc because dicom11, dicom12, ... will come before dicom2.
     * Also our only indication of the "true" ordering is slice location.
     * 
     * @param A Array to be sorted.
     * @param B Array it goes into.
     * @param size Size of the array (both arrays are the same size).
     * 
     * @return <code>false</code> only if any of the numbers in the array are equal.
     */
    private static boolean sort(final float[] A, final int[] B, final int size) {
        
        LinkedNum[] n = new LinkedNum[size];
        for(int i=0; i<size; i++) {
            n[i] = new LinkedNum(A[i], B[i]);
        }
        
        Arrays.sort(n, new LinkedComparator());
        
        for(int i=0; i<size; i++) {
            B[i] = n[i].index;
            A[i] = n[i].num;
        }
        
        for(int i=0; i<size-1; i++) {
            if(A[i] == A[i+1]) {
                return false;
            }
        }
        
        return true;
    }
    
    static class LinkedNum {
        float num;
        int index;
        
        public LinkedNum(float num, int index) {
            this.num = num;
            this.index = index;
        }
    }
    
    private static class LinkedComparator implements Comparator<LinkedNum> {
        public int compare(LinkedNum arg0, LinkedNum arg1) {
            return (int) (arg0.num - arg1.num);
        }
        
    }

    /**
     * Sorts an array of ints (using insertion sort), turns into array of ints used to sort images by image number;
     * thus, the final value of B[i] is for the ith image read in, put that image at location B[i] in the image buffer.
     * This is necessary for images labeled dicom1, dicom2, etc because dicom11, dicom12, ... will come before dicom2.
     * Also our only indication of the "true" ordering is image number.
     * 
     * @param A Array to be sorted.
     * @param B Array it goes into.
     * @param size Size of the array (both arrays are the same size).
     * 
     * @return <code>false</code> only if any of the numbers in the array are equal.
     */
    private static boolean sort(final int[] A, final int[] B, final int size) {
        boolean flag = true;
        int stop = size - 1, i, tmp2;
        int tmp;

        while (stop > 0) {

            for (i = 0; i < stop; i++) {

                if (A[i] > A[i + 1]) {
                    tmp = A[i];
                    A[i] = A[i + 1];
                    A[i + 1] = tmp;
                    tmp2 = B[i];
                    B[i] = B[i + 1];
                    B[i + 1] = tmp2;
                }

                if (A[i] == A[i + 1]) {
                    flag = false;
                }
            }

            stop--;
        }

        final int[] C = new int[size];

        for (i = 0; i < size; i++) {
            C[B[i]] = i;
        }

        for (i = 0; i < size; i++) {
            B[i] = C[i];
        }

        return flag;
    }

    /**
     * Helper method for finding biggest (absolute value) of three numbers.
     * 
     * @param zero First value
     * @param one Second value
     * @param two Third value
     * 
     * @return Index of argument that is the biggest. That is, if <code>zero</code> is the largest of the three
     *         numbers, the value 0 is returned; alternatively, if the second argument is the largest of the three
     *         numbers, the value 1 is returned; and so on.
     */
    private int absBiggest(final double zero, final double one, final double two) {

        if (Math.abs(zero) > Math.abs(one)) {

            if (Math.abs(zero) > Math.abs(two)) {
                return 0;
            } else {
                return 2;
            }
        } else {

            if (Math.abs(one) > Math.abs(two)) {
                return 1;
            } else {
                return 2;
            }
        }
    }

    /**
     * Calls GUI dialogs based on what type of image this is and the number of dimensions.
     * 
     * @param extents Extents of the image, used to determine what type of dialog to call and initialize variables.
     * @param isTiff Flag indicating if this is a TIFF file; TIFF files require more options.
     * @param options Options to get from the dialog and save.
     * 
     * @return DOCUMENT ME!
     */
    private boolean callDialog(final int[] extents, final boolean isTiff, FileWriteOptions options) {

        JDialogSaveSlices dialogSave = null;

        if ( (extents.length == 2) && isTiff && options.isPackBitEnabled()) {
            final int response = JOptionPane.showConfirmDialog(UI.getMainFrame(), "Save with pack bit compression?",
                    "Compression", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);

            if (response == JOptionPane.YES_OPTION) {
                options.setWritePackBit(true);
                options.setDefault(false);
            } else {
                options.setWritePackBit(false);
            }
        } else if (extents.length >= 2) {
            if ( (extents.length == 2) && (options.getFileType() == FileUtility.DICOM) && displayRangeOfSlicesDialog) {
                // we want the saveslices to pop up in this case so the user can select if they want
                // to save as encapsulated jpeg2000..unless ofcourse flag has been set to false
                dialogSave = new JDialogSaveSlices(UI.getMainFrame(), 0, 0, options);
            } else if (extents.length == 3) {
                dialogSave = new JDialogSaveSlices(UI.getMainFrame(), 0, extents[2] - 1, options);
            } else if (extents.length == 4) {
                dialogSave = new JDialogSaveSlices(UI.getMainFrame(), 0, extents[2] - 1, 0, extents[3] - 1, options);
            } else {
                return true;
            }

            if (dialogSave.isCancelled()) {
                return false;
            }

            saveAsEncapJP2 = dialogSave.getSaveAsEncapJP2();
            options = dialogSave.getWriteOptions();
            options.doStamp(dialogSave.doStampSecondary());
            options.doEnhanced(dialogSave.doSaveEnhancedDicom());

            if (extents.length == 3) {

                if ( ! ( (options.getBeginSlice() == 0) && (options.getEndSlice() == (extents[2] - 1)))) {
                    options.setDefault(false);
                }

                if (options.isMultiFile()) {
                    options.setDefault(false);
                }
            } else if (extents.length == 4) {

                if ( (options.getFileType() == FileUtility.TIFF) || (options.getFileType() == FileUtility.MINC)) {

                    if ( ! ( (options.getBeginSlice() == 0) && (options.getEndSlice() == (extents[2] - 1)) && (options
                            .getTimeSlice() == 0))) {
                        options.setDefault(false);
                    }

                    if (options.isMultiFile()) {
                        options.setDefault(false);
                    }
                } else {

                    // if these are the defaults, don't append them, it's not necessary. otherwise will append all 4.
                    if ( ! ( (options.getBeginSlice() == 0) && (options.getEndSlice() == (extents[2] - 1))
                            && (options.getBeginTime() == 0) && (options.getEndTime() == (extents[3] - 1)))) {
                        options.setDefault(false);
                    }
                }
            }
        }

        return true;
    }

    /**
     * Creates the progress bar and links (if not null) the progress bar to a FileBase for reading/writing if the
     * progress bar should not be updated within the FileBase's readImage/writeImage, pass in null to the fBase and
     * update within FileIO's read[ImageType] or write[ImageType] methods.
     * 
     * @param fBase the FileBase that will add the Progress Bar as a listener (for fireProgressStateChanged())
     * @param fName the filename (will be displayed in the title and part of the message)
     * @param message this message should FILE_READ, FILE_OPEN
     */
    private void createProgressBar(final FileBase fBase, final String fName, final String message) {

        // progressBar = new ViewJProgressBar(fName, message + fName + " ...", 0, 100, true);
        // progressBar.setVisible(ViewUserInterface.getReference().isAppFrameVisible() && !quiet);

        // the quiet flag is needed to determine if progress bar is visible or not
        if ( !GraphicsEnvironment.isHeadless()) {
            progressBar = new ViewJProgressBar(fName, message + fName + " ...", 0, 100, true, null, null, !quiet);
            progressBar.progressStateChanged(new ProgressChangeEvent(this, 0, null, null));
        }

        if (fBase != null && progressBar != null) {
            fBase.addProgressChangeListener(progressBar);
        }
    }

    /**
     * Reads an AFNI file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param loadB true if loading imageB
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readAfni(final String fileName, final String fileDir, final boolean loadB) {
        ModelImage image = null;
        FileAfni imageFile;
        final boolean doRead = true;

        try {
            imageFile = new FileAfni(fileName, fileDir, loadB, doRead);
            createProgressBar(imageFile, fileName, FileIO.FILE_READ);
            image = imageFile.readImage();
            // LUT = imageFile.getModelLUT();
        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;

    }

    /**
     * Reads an analyze file by calling the read method of the file. Also checks if it's a Cheshire and if so, calls
     * that method instead. This method contains special code to not display the progress bar should the image be
     * <q>splash.img</q>.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param one Indicates that only the named file should be read, as opposed to reading the matching files in the
     *            directory, as defined by the filetype. <code>true</code> if only want to read one image from 3D
     *            dataset.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readAnalyze(final String fileName, final String fileDir, final boolean one) {
        ModelImage image = null;
        FileAnalyze imageFile = null;

        if (FileCheshire.isCheshire(fileName, fileDir)) {
            image = readCheshire(fileName, fileDir);
        } else {

            // most likely an Analyze file
            try {
                imageFile = new FileAnalyze(fileName, fileDir);
                createProgressBar(imageFile, fileName, FileIO.FILE_READ);
                image = imageFile.readImage(one);
            } catch (final IOException error) {

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                error.printStackTrace();

                return null;
            } catch (final OutOfMemoryError error) {

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                error.printStackTrace();

                return null;
            }
        }

        if (imageFile != null) {
            imageFile.finalize();
            imageFile = null;
        }

        return image;
    }

    /**
     * Reads an siemenstext file by calling the read method of the file. Also checks if it's a Cheshire and if so, calls
     * that method instead. This method contains special code to not display the progress bar should the image be
     * <q>splash.img</q>.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param one Indicates that only the named file should be read, as opposed to reading the matching files in the
     *            directory, as defined by the filetype. <code>true</code> if only want to read one image from 3D
     *            dataset.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readSiemensText(final String fileName, final String fileDir, final boolean one) {
        ModelImage image = null;
        FileSiemensText imageFile = null;

        if (FileCheshire.isCheshire(fileName, fileDir)) {
            image = readCheshire(fileName, fileDir);
        } else {

            // most likely an SiemensText file
            try {
                imageFile = new FileSiemensText(fileName, fileDir);
                createProgressBar(imageFile, fileName, FileIO.FILE_READ);
                image = imageFile.readImage(one);
            } catch (final IOException error) {

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                error.printStackTrace();

                return null;
            } catch (final OutOfMemoryError error) {

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                error.printStackTrace();

                return null;
            }
        }

        if (imageFile != null) {
            imageFile.finalize();
            imageFile = null;
        }

        return image;
    }

    /**
     * Reads a multi Analyze file. Gets a list of the images from the file directory and reads them each in.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readAnalyzeMulti(final String fileName, final String fileDir) {
        ModelImage image = null;
        FileAnalyze imageFile = null;
        FileInfoBase fileInfo;

        int length = 0;
        float[] buffer;
        String[] fileList;
        int[] extents;
        float[] resolutions;
        int nImages;
        // of proper extents (in case there is a file with the consistent filename but
        // inconsistent extents.) we do assume the 1st header is correct

        int i = 0;

        try {
            fileList = FileUtility.getFileList(fileDir, fileName, quiet);

            for (final String element : fileList) {

                if (element != null) {

                    // System.out.println(" Name = " + fileList[m]);
                    i++;
                }
            }
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        nImages = i; // total number of suspected files to import into an image

        if (nImages == 1) {
            return readAnalyze(fileName, fileDir, false);
        }

        createProgressBar(null, fileName, FileIO.FILE_READ);

        // if one of the images has the wrong extents, the following must be changed.
        // (ei., too many images!)
        // for simplicity of setup, read in the first file hdr
        imageFile = new FileAnalyze(fileList[0], fileDir);

        try {

            if ( !imageFile.readHeader(fileList[0], fileDir)) {
                throw (new IOException(" Analyze header file error"));
            }
        } catch (final IOException ioe) {

            if ( !quiet) {
                MipavUtil.displayError("Error reading header file.");
            }

            ioe.printStackTrace();
        }

        fileInfo = (imageFile).getFileInfo();
        extents = fileInfo.getExtents();
        resolutions = fileInfo.getResolutions();

        if (extents.length == 2) {
            length = extents[0] * extents[1];
        } else if (extents.length == 3) {
            length = extents[0] * extents[1] * extents[2];
        } else if (extents.length == 4) {
            length = extents[0] * extents[1] * extents[2] * extents[3];
        }

        buffer = new float[length];

        final int[] imgExtents = new int[extents.length + 1];
        final float[] imgResolutions = new float[resolutions.length + 1]; // should be same size as extents.

        // copy proper values into img extents, assuming that the 1st (numerically indexed) images
        // is correct to begin with
        for (i = 0; i < extents.length; i++) {
            imgExtents[i] = extents[i];
            imgResolutions[i] = resolutions[i];
            // set the number of slices in the image later.
        }

        imgExtents[i] = nImages; // may not be right, but we'll find out after we go through all images.
        imgResolutions[i] = 1; // resolution in the created axis is not physically defined; is generated.

        image = new ModelImage(fileInfo.getDataType(), imgExtents, fileInfo.getFileName());
        imageFile.finalize();
        imageFile = null;

        int imageCount = 0;
        int fInfoCount = 0;

        // loop through image and store data in image model
        for (i = 0; i < nImages; i++) {

            try {

                // progressBar.setTitle(UI.getProgressBarPrefix() + "image " + fileList[i]);
                progressBar.updateValueImmed(Math.round((float) i / (nImages - 1) * 100));
                imageFile = new FileAnalyze(fileList[i], fileDir);

                if ( ! (imageFile).readHeader(fileList[i], fileDir)) {
                    throw (new IOException(" Analyze header file error"));
                }

                // chk the extents of the image to verify it is consistent
                // (this doesn't ensure there won't be null exceptions@)
                fileInfo = (imageFile).getFileInfo();

                if (extents.length != fileInfo.getExtents().length) {

                    if ( !quiet) {
                        MipavUtil
                                .displayError("Inconsistent analyze image file found.  This File will be skipped.  The number of dimensions does not match.");
                    }

                    continue;
                } else { // the prototype image and the read-in image are of the same dimension....

                    switch (extents.length) { // check that they extend as far in all dimensions:

                        case 2:
                            if ( (extents[0] != fileInfo.getExtents()[0]) || (extents[1] != fileInfo.getExtents()[1])) {

                                if ( !quiet) {
                                    MipavUtil
                                            .displayError("Inconsistent analyze image file found.  This File will be skipped.  One or more of the X-Y dimensions do not match.");
                                }

                                continue;
                            }

                            break;

                        case 3:
                            if ( (extents[0] != fileInfo.getExtents()[0]) || (extents[1] != fileInfo.getExtents()[1])
                                    || (extents[2] != fileInfo.getExtents()[2])) {

                                if ( !quiet) {
                                    MipavUtil
                                            .displayError("Inconsistent analyze image file found.  This File will be skipped.  One or more of the X-Y-Z dimensions do not match.");
                                }

                                continue;
                            }

                            break;

                        case 4:
                            if ( (extents[0] != fileInfo.getExtents()[0]) || (extents[1] != fileInfo.getExtents()[1])
                                    || (extents[2] != fileInfo.getExtents()[2])
                                    || (extents[3] != fileInfo.getExtents()[3])) {

                                if ( !quiet) {
                                    MipavUtil
                                            .displayError("Inconsistent analyze image file found.  This File will be skipped.  One or more of the X-Y-Z-T dimensions do not match.");
                                }

                                continue;
                            }

                            break;

                        default:
                            break;
                    }
                }

                fileInfo.setExtents(imgExtents); // set image extents to proper value!
                fileInfo.setResolutions(imgResolutions);

                if (nImages > 1) {
                    fileInfo.setMultiFile(true);
                }

                (imageFile).readImage(buffer);
                image.importData(imageCount * length, buffer, false);

                if (image.getExtents().length > 3) {

                    for (int j = 0; j < image.getExtents()[2]; j++) {
                        image.setFileInfo(fileInfo, fInfoCount);
                        fInfoCount++;
                    }
                } else {
                    image.setFileInfo(fileInfo, imageCount);
                }

                // image.setFileInfo(fileInfo, imageCount);
                imageCount++; // image was okay, so count it.(can't do it before b/c of offset)
            } catch (final IOException error) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                error.printStackTrace();

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (final ArrayIndexOutOfBoundsException error) {

                if ( !quiet) {
                    MipavUtil.displayError("Unable to read images: the image\nnumber in the file "
                            + fileInfo.getFileName() + " is corrupted.");
                }

                error.printStackTrace();

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (final OutOfMemoryError error) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                error.printStackTrace();

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            }
            imageFile.finalize();
            imageFile = null;
        }

        // i goes 1 too far anyway, but if we skipped files, be sure to account for it,
        // because our basic model was that all prperly named files were good analyze images.
        // only we found one or more didn't fit. We must now take that into account.
        // ie., we read in imageCount # of images, we expected nImages.
        if (imageCount < nImages) {
            final FileInfoBase[] fileInfoArr = image.getFileInfo();

            imgExtents[image.getNDims() - 1] = imageCount; // last dimension available should be the num images read

            final int sliceSize = buffer.length;

            buffer = new float[sliceSize * imageCount];

            try {
                image.exportData(0, buffer.length, buffer); // copy all buffer
                image.reallocate(fileInfo.getDataType(), imgExtents);
                image.importData(0, buffer, true); // remake the model image with the right number of slices.

                for (i = 0; i < imgExtents[imgExtents.length - 1]; i++) { // copy all image info
                    fileInfoArr[i].setExtents(imgExtents); // update extents
                    image.setFileInfo(fileInfoArr[i], i); // copying...
                }
            } catch (final IOException ioe) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO reports: " + ioe.getMessage());
                }

                return null;
            }

            final FileInfoBase[] fileInfoArrCopy = new FileInfoBase[imgExtents[imgExtents.length - 1]];

            for (i = 0; i < imgExtents[imgExtents.length - 1]; i++) { // copy all image info
                fileInfoArr[i].setExtents(imgExtents); // update extents
                fileInfoArrCopy[i] = fileInfoArr[i];
            }

            image.setFileInfo(fileInfoArrCopy);
        }

        return image;

    }

    /**
     * Reads an AVI file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param one Indicates that only the named file should be read, as opposed to reading the matching files in the
     *            directory, as defined by the filetype. <code>true</code> if only want to read one image from 3D
     *            dataset.
     * @param readQT Indicates that a QuickTime movie file is being read. <code>true</code> if this file represents
     *            QuickTime.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readAvi(final String fileName, final String fileDir, final boolean one, final boolean readQT) {
        FileAvi imageFile;
        ModelImage image = null;

        try {
            imageFile = new FileAvi(fileName, fileDir);
            imageFile.setReadQT(readQT);
            // imageFile.setProgressBar(pInterface);

            createProgressBar(imageFile, fileName, FileIO.FILE_READ);
            image = imageFile.readImage(one);

            // LUT used in compressed RLE8 files
            LUT = imageFile.getModelLUT();
        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;

    }

    /**
     * Reads a BioRad PIC file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param one Indicates that only the named file should be read, as opposed to reading the matching files in the
     *            directory, as defined by the filetype. <code>true</code> if only want to read one image from 3D
     *            dataset.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readBioRad(final String fileName, final String fileDir, final boolean one) {
        ModelImage image = null;
        FileBioRad imageFile;

        try {
            imageFile = new FileBioRad(fileName, fileDir);
            image = imageFile.readImage(one);
            // LUT = imageFile.getModelLUT();
        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;

    }

    /**
     * Reads a BRUKER file by first reading in the d3proc header file, second the reco header file, third the acqp file,
     * and finally the 2dseq binary file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param one Indicates that only the named file should be read, as opposed to reading the matching files in the
     *            directory, as defined by the filetype. <code>true</code> if only want to read one image from 3D
     *            dataset.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readBRUKER(String fileName, final String fileDir, final boolean one) {
        ModelImage image = null;
        FileBRUKER imageFile = null;
        FileInfoBase myFileInfo;
        File directoryFile;
        String workingDirectory; // refers to loaction of 2dseq, d3proc, and reco file
        String parentDirectoryName;

        try {
            fileName = "d3proc";
            imageFile = new FileBRUKER(fileName, fileDir); // read in files
            workingDirectory = imageFile.getFileDir();
            imageFile.readd3proc();
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("d3proc FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("d3proc Out of memory: " + error);
            }

            error.printStackTrace();

            return null;
        }

        imageFile.setFileName("reco");

        try {
            imageFile.readreco();
        } catch (final IOException error) {
            myFileInfo = imageFile.getFileInfo();
            myFileInfo.setEndianess(FileBase.BIG_ENDIAN);
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("reco out of memory: " + error);
            }

            error.printStackTrace();

            return null;
        }

        imageFile.setFileName("acqp");

        final File tmpFile = new File(workingDirectory + File.separator + "acqp");

        if ( !tmpFile.exists()) {

            // go up 2 parent directories
            parentDirectoryName = new File(workingDirectory).getParent();
            directoryFile = new File(parentDirectoryName);
            imageFile.setFileDir(directoryFile.getParent() + File.separator);
        } else {
            imageFile.setFileDir(workingDirectory + File.separator);
        }

        try {
            imageFile.readacqp();
            imageFile.setFileName("method"); // must reside in same location as acqp
            final File methodFile = new File(imageFile.getFileDir() + File.separator + "method");
            if (methodFile.exists()) {
                imageFile.readMethod();
            }

        } catch (final IOException error) {
            Preferences.debug("IOExceoption in FileIO.readBRUKER\n", Preferences.DEBUG_FILEIO);

            if ( !quiet) {
                MipavUtil.displayError("IOException in FileIO.readBRUKER: " + error);
            }

            return null;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("acqp out of memory: " + error);
            }

            return null;
        }

        imageFile.setFileName("2dseq");
        imageFile.setFileDir(workingDirectory);

        try {
            image = imageFile.readImage(one);
        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("Out of memory: " + error);
            }

            error.printStackTrace();

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;
    }

    /**
     * Reads a Cheshire file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readCheshire(final String fileName, final String fileDir) {
        ModelImage image = null;
        FileCheshire imageFile;

        try {
            imageFile = new FileCheshire(fileName, fileDir, true);
            createProgressBar(imageFile, fileName, FileIO.FILE_READ);
            image = imageFile.readImage();
        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;
    }

    /**
     * Reads a COR file by first reading the header file then reading in each separate slice file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readCOR(String fileName, final String fileDir) {
        int[] extents; // extent of image (!def!)
        int length = 0;
        int i;

        // float[] buffer;
        FileInfoBase myFileInfo;
        String[] fileList = null;
        FileCOR imageFile = null;
        ModelImage image = null;

        String origName = null;
        boolean tryAgain = false;
        int nImages;

        // First try .info, then try .info~, then try original name
        fileList = FileUtility.getCORFileList(fileDir, fileName, quiet); // get series of files in the chosen dir
        nImages = fileList.length;

        try {
            origName = fileName;
            fileName = FileUtility.trimCOR(fileName) + ".info"; // allow user to click on any file in set
            imageFile = new FileCOR(fileName, fileDir); // read in files
            imageFile.readInfoImage();
        } catch (final IOException error) {
            tryAgain = true;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("Out of memory: " + error);
            }

            error.printStackTrace();

            return null;
        }

        if (tryAgain) {
            tryAgain = false;

            try {
                fileName = FileUtility.trimCOR(origName) + ".info~";
                imageFile = new FileCOR(fileName, fileDir); // read in files
                imageFile.readInfoImage();
            } catch (final IOException error) {
                tryAgain = true;
            } catch (final OutOfMemoryError error) {

                if ( !quiet) {
                    MipavUtil.displayError("Out of memory: " + error);
                }

                error.printStackTrace();

                return null;
            }
        } // if (tryAgain)

        if (tryAgain) {

            try {
                fileName = origName;
                imageFile = new FileCOR(fileName, fileDir); // read in files
                imageFile.readInfoImage();
            } catch (final IOException error) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                error.printStackTrace();

                return null;
            } catch (final OutOfMemoryError error) {

                if ( !quiet) {
                    MipavUtil.displayError("Out of memory: " + error);
                }

                error.printStackTrace();

                return null;
            }
        } // if (tryAgain)

        myFileInfo = imageFile.getFileInfo();

        length = myFileInfo.getExtents()[0] * myFileInfo.getExtents()[1];

        try {

            if (nImages > 1) {
                extents = new int[3];
                extents[2] = nImages;
            } else {
                extents = new int[2];
            }

            extents[0] = myFileInfo.getExtents()[0]; // copy out current [0,1] coords
            extents[1] = myFileInfo.getExtents()[1]; // so all 3 ([0,1,2]) may be added

            image = new ModelImage(myFileInfo.getDataType(), extents, myFileInfo.getFileName());

        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("Out of memory: " + error);
            }

            error.printStackTrace();

            return null;
        }

        image.setFileInfo(myFileInfo, 0);

        try {
            imageFile = new FileCOR(fileList[0], fileDir);
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            return null;
        }

        createProgressBar(imageFile, fileName, FileIO.FILE_READ);

        // loop through image and store data in image model
        for (i = 0; i < nImages; i++) {

            try {

                // progressBar.setTitle(UI.getProgressBarPrefix() + "image " + fileList[i]);
                progressBar.updateValueImmed(Math.round((float) i / (nImages - 1) * 100));
                (imageFile).setFileName(fileList[i]);
                (imageFile).readImage(length);
                image.setFileInfo(myFileInfo, i);

                if (image.isColorImage()) {
                    image.importData(i * 4 * length, (imageFile).getImageBuffer(), false);
                } else {
                    image.importData(i * length, (imageFile).getImageBuffer(), false);
                }
            } catch (final IOException error) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                error.printStackTrace();

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (final ArrayIndexOutOfBoundsException error) {

                if ( !quiet) {
                    MipavUtil.displayError("Unable to read images: the image\n" + "number in the file "
                            + myFileInfo.getFileName() + " is corrupted.");
                }

                error.printStackTrace();

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (final OutOfMemoryError error) {

                if ( !quiet) {
                    MipavUtil.displayError("Out of memory: " + error);
                }

                error.printStackTrace();

                if (image != null) {

                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            }
        }

        imageFile.finalize();
        imageFile = null;
        return image;

    }

    /**
     * Reads a Dm3 file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param one Indicates that only the named file should be read, as opposed to reading the matching files in the
     *            directory, as defined by the filetype. <code>true</code> if only want to read one image from 3D
     *            dataset.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readDM3(final String fileName, final String fileDir, final boolean one) {
        ModelImage image = null;
        FileDM3 imageFile;

        try {
            imageFile = new FileDM3(fileName, fileDir);
            image = imageFile.readImage(one);
            // LUT = imageFile.getModelLUT();
        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;

    }

    /**
     * Reads a FITS file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param one Indicates that only the named file should be read, as opposed to reading the matching files in the
     *            directory, as defined by the filetype. <code>true</code> if only want to read one image from 3D
     *            dataset.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readFits(final String fileName, final String fileDir, final boolean one) {
        ModelImage image = null;
        FileFits imageFile;

        try {
            imageFile = new FileFits(fileName, fileDir);
            createProgressBar(imageFile, fileName, FileIO.FILE_READ);
            image = imageFile.readImage(one);
            // LUT = imageFile.getModelLUT();
        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;

    }

    /**
     * Reads in multiple GE Genesis 5x type files.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * 
     * @return The image that was read in, or null if failure.
     */
    public ModelImage readGEGenesis5XMulti(final String fileName, final String fileDir) {

        ModelImage image = null;
        FileGESigna5X imageFile;
        FileInfoGESigna5X fileInfo[] = null;
        String[] fileList;
        float[] buffer;
        int[] extents;
        int length = 0;
        int i;
        int imageSize = 0;
        int width, height;
        int nImages;
        int[] orient = {0, 0, 0};
        float slice0Pos = 0.0f;
        float slice1Pos = 0.0f;

        try {

            fileList = FileUtility.getFileList(fileDir, fileName, quiet);
            imageFile = new FileGESigna5X(fileName, fileDir);

            imageFile.setFileName(fileList[0]);
            imageSize = imageFile.readImageFileData();

            if (imageSize == -1) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: Compression not supported for Signa 5X");
                }

                return null;
            }

            if (imageSize == -2) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: Not a Signa 5X file.");
                }

                return null;
            }

            if (imageSize == 0) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: Image length.");
                }

                return null;
            }

            createProgressBar(null, fileName, FileIO.FILE_READ);

            width = imageFile.getWidth();
            height = imageFile.getHeight();
            length = width * height;
            buffer = new float[length];

            if (fileList.length == 1) {
                extents = new int[2];
                extents[0] = width;
                extents[1] = height;
                fileInfo = new FileInfoGESigna5X[1];
            } else {
                extents = new int[3];
                extents[0] = width;
                extents[1] = height;
                extents[2] = fileList.length;
                fileInfo = new FileInfoGESigna5X[extents[2]];
            }

            image = new ModelImage(ModelStorageBase.USHORT, extents, "GE");
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            if (progressBar != null) {}

            return null;
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            if (progressBar != null) {}

            return null;
        }

        /*
         * if (imageFile.getStartAdjust() > 0) { nImages = 1; } else { nImages = fileList.length; }
         */
        nImages = fileList.length;

        // loop through files, place them in image array
        for (i = 0; i < nImages; i++) {

            try {
                progressBar.setTitle(UI.getProgressBarPrefix() + "image " + fileList[i]);
                progressBar.updateValueImmed(Math.round((float) i / (nImages - 1) * 100));

                if (fileList[i] != null) {
                    imageFile.setFileName(fileList[i]);
                    imageFile.readImageFileData();
                    imageFile.readImage(buffer);

                    fileInfo[i] = (FileInfoGESigna5X) imageFile.getFileInfo().clone(); // Needed to set index

                    if (i == 0) {
                        orient = fileInfo[0].getAxisOrientation();

                        switch (fileInfo[0].getImageOrientation()) {

                            case FileInfoBase.AXIAL:
                                slice0Pos = fileInfo[0].imgTLHC_S;
                                break;

                            case FileInfoBase.CORONAL:
                                slice0Pos = fileInfo[0].imgTLHC_A;
                                break;

                            case FileInfoBase.SAGITTAL:
                                slice0Pos = fileInfo[0].imgTLHC_R;
                                break;
                        }
                    } // if (i == 0)
                    else if (i == 1) {
                        orient = fileInfo[1].getAxisOrientation();

                        switch (fileInfo[1].getImageOrientation()) {

                            case FileInfoBase.AXIAL:
                                slice1Pos = fileInfo[1].imgTLHC_S;
                                if (slice1Pos > slice0Pos) {
                                    orient[2] = FileInfoBase.ORI_I2S_TYPE;
                                } else {
                                    orient[2] = FileInfoBase.ORI_S2I_TYPE;
                                }

                                break;

                            case FileInfoBase.CORONAL:
                                slice1Pos = fileInfo[1].imgTLHC_A;
                                if (slice1Pos > slice0Pos) {
                                    orient[2] = FileInfoBase.ORI_P2A_TYPE;
                                } else {
                                    orient[2] = FileInfoBase.ORI_A2P_TYPE;
                                }

                                break;

                            case FileInfoBase.SAGITTAL:
                                slice1Pos = fileInfo[1].imgTLHC_R;
                                if (slice1Pos > slice0Pos) {
                                    orient[2] = FileInfoBase.ORI_L2R_TYPE;
                                } else {
                                    orient[2] = FileInfoBase.ORI_R2L_TYPE;
                                }

                                break;
                        } // switch (fileInfo[1].getImageOrientation())

                        if (fileInfo[0] != null) {
                            image.setFileInfo(fileInfo[0], 0);
                            fileInfo[0].setAxisOrientation(orient);
                        }
                    } // else if (i == 1)

                    if (i != 0) {
                        fileInfo[i].setAxisOrientation(orient);
                    }

                    fileInfo[i].setExtents(extents);
                    image.setFileInfo(fileInfo[i], i);
                    image.importData(i * length, buffer, false);
                } // if (fileList[i] != null)
            } // try
            catch (final IOException error) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                error.printStackTrace();

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (final OutOfMemoryError error) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                error.printStackTrace();

                if (image != null) {

                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            }
        } // for (i = 0; i < nImages; i++)

        image.setImageName(imageFile.getFileInfo().getImageNameFromInfo(), false);

        imageFile.finalize();
        imageFile = null;
        return image;
    }

    /**
     * Reads in multiple GE Signa 4x type files.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readGESigna4XMulti(final String fileName, final String fileDir) {

        ModelImage image = null;
        FileGESigna4X imageFile;
        FileInfoBase myFileInfo0 = null;
        FileInfoBase myFileInfo;
        String[] fileList;
        float[] buffer;
        int[] extents;
        int length = 0;
        int i;
        int width, height;
        int nImages;
        int imageSize;
        int[] orient = {0, 0, 0};
        float slice0Pos = 0.0f;
        float slice1Pos = 0.0f;

        try {

            fileList = FileUtility.getFileList(fileDir, fileName, quiet);
            imageFile = new FileGESigna4X(fileName, fileDir);

            imageFile.setFileName(fileList[0]);
            imageSize = imageFile.readImageFileData();

            if (imageSize == -1) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: Compression not supported for Signa 4X");
                }

                return null;
            }

            if (imageSize == -2) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: Not a Signa 4X file.");
                }

                return null;
            }

            if (imageSize == 0) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: Image length.");
                }

                return null;
            }

            width = imageFile.getWidth();
            height = imageFile.getHeight();
            length = width * height;
            buffer = new float[length];

            if (fileList.length == 1) {
                extents = new int[2];
                extents[0] = width;
                extents[1] = height;
            } else {
                extents = new int[3];
                extents[0] = width;
                extents[1] = height;
                extents[2] = fileList.length;
            }

            image = new ModelImage(ModelStorageBase.USHORT, extents, "GE");
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        nImages = fileList.length;

        createProgressBar(null, fileName, FileIO.FILE_READ);

        // loop through files, place them in image array
        for (i = 0; i < nImages; i++) {

            try {

                // progressBar.setTitle(UI.getProgressBarPrefix() + "image " + fileList[i]);
                progressBar.updateValueImmed(Math.round((float) i / (nImages - 1) * 100));

                if (fileList[i] != null) {
                    imageFile.setFileName(fileList[i]);
                    imageFile.readImageFileData();
                    imageFile.readImage(buffer);

                    myFileInfo = imageFile.getFileInfo(); // Needed to set index

                    if (i == 0) {
                        myFileInfo0 = imageFile.getFileInfo();
                        orient = myFileInfo.getAxisOrientation();

                        switch (myFileInfo.getImageOrientation()) {

                            case FileInfoBase.AXIAL:
                                slice0Pos = imageFile.getImgTLHC_S();
                                break;

                            case FileInfoBase.CORONAL:
                                slice0Pos = imageFile.getImgTLHC_A();
                                break;

                            case FileInfoBase.SAGITTAL:
                                slice0Pos = imageFile.getImgTLHC_R();
                                break;
                        }
                    } // if (i == 0)
                    else if (i == 1) {
                        orient = myFileInfo.getAxisOrientation();

                        switch (myFileInfo.getImageOrientation()) {

                            case FileInfoBase.AXIAL:
                                slice1Pos = imageFile.getImgTLHC_S();
                                if (slice1Pos > slice0Pos) {
                                    orient[2] = FileInfoBase.ORI_I2S_TYPE;
                                } else {
                                    orient[2] = FileInfoBase.ORI_S2I_TYPE;
                                }

                                break;

                            case FileInfoBase.CORONAL:
                                slice1Pos = imageFile.getImgTLHC_A();
                                if (slice1Pos > slice0Pos) {
                                    orient[2] = FileInfoBase.ORI_P2A_TYPE;
                                } else {
                                    orient[2] = FileInfoBase.ORI_A2P_TYPE;
                                }

                                break;

                            case FileInfoBase.SAGITTAL:
                                slice1Pos = imageFile.getImgTLHC_R();
                                if (slice1Pos > slice0Pos) {
                                    orient[2] = FileInfoBase.ORI_L2R_TYPE;
                                } else {
                                    orient[2] = FileInfoBase.ORI_R2L_TYPE;
                                }

                                break;
                        } // switch (myFileInfo.getImageOrientation())

                        if (myFileInfo0 != null) {
                            image.setFileInfo(myFileInfo0, 0);
                            myFileInfo0.setAxisOrientation(orient);
                        }
                    } // else if (i == 1)

                    if (i != 0) {
                        myFileInfo.setAxisOrientation(orient);
                    }

                    myFileInfo.setExtents(extents);
                    myFileInfo.setOrigin( ((FileInfoGESigna4X) (myFileInfo)).getOriginAtSlice(imageFile
                            .getImageNumber() - 1));

                    image.setFileInfo(myFileInfo, imageFile.getImageNumber() - 1);
                    image.importData( (imageFile.getImageNumber() - 1) * length, buffer, false);
                } // if (fileList[i] != null)
            } // try
            catch (final IOException error) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                error.printStackTrace();

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (final OutOfMemoryError error) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                error.printStackTrace();

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            }
        } // for (i = 0; i < nImages; i++)

        image.setImageName(imageFile.getFileInfo().getImageNameFromInfo(), false);
        imageFile.finalize();
        imageFile = null;

        return image;
    }

    /**
     * Reads an ICS file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readICS(final String fileName, final String fileDir) {
        ModelImage image = null;
        FileICS imageFile;

        try {
            imageFile = new FileICS(fileName, fileDir);
            createProgressBar(imageFile, fileName, FileIO.FILE_READ);
            image = imageFile.readImage();
            // LUT = imageFile.getModelLUT();
        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;

    }

    /**
     * Reads an Interfile file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param one Indicates that only the named file should be read, as opposed to reading the matching files in the
     *            directory, as defined by the filetype. <code>true</code> if only want to read one image from 3D
     *            dataset.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readInterfile(final String fileName, final String fileDir, final boolean one) {
        ModelImage image = null;
        FileInterfile imageFile;

        try {
            imageFile = new FileInterfile(fileName, fileDir);
            createProgressBar(imageFile, fileName, FileIO.FILE_READ);
            image = imageFile.readImage(one);
            // LUT = imageFile.getModelLUT();
        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;

    }

    /**
     * Reads a JIMI file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param multifile Indication whether this file is be read alone, or if the reader is to read all matching
     *            filenames as a part of the dataset. <code>true</code> if want to read all matching files to form an
     *            image into a 3D dataset.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readJimi(final String fileName, final String fileDir, final boolean multifile) {
        ModelImage modelImage = null;
        Image image = null;
        int imageWidth = 0;
        int imageHeight = 0;
        int[] extents;
        int[] buffer = null;
        int[] greyBuffer = null;
        String[] fileList;

        if (multifile) {
            fileList = FileUtility.getFileList(fileDir, fileName, quiet);
        } else {
            fileList = new String[] {fileName};
        }

        final MediaTracker mediaTracker = new MediaTracker(UI.getMainFrame());
        extents = new int[] {0, 0, fileList.length};

        for (int j = 0; j < fileList.length; j++) {

            try {

                mediaTracker.addImage(image, 0);

                boolean loaded = mediaTracker.waitForAll(20000);

                if ( !loaded || (image == null)) {

                    try {
                        image = ImageIO.read(new File(fileDir + fileList[j])); // if JIMI fails, try this

                        // String[] readTypes = ImageIO.getReaderFormatNames();

                        // for (int t = 0; t < readTypes.length; t++) {
                        // System.out.println("ImageIO read formats: " + readTypes[t]);
                        // }

                        // String[] writeTypes = ImageIO.getWriterFormatNames();

                        // for (int t = 0; t < writeTypes.length; t++) {
                        // System.out.println("ImageIO write formats: " + writeTypes[t]);
                        // }

                    } catch (final IOException ioe) {
                        // intentionally empty
                    }
                }

                try {

                    if (image == null) {

                        // This is odd Jimi seems to not stop after the application is closed - at least under the
                        // development environment - must test when running the application. -- Matt 3/16/07
                        image = Jimi.getImage(fileDir + fileList[j]); // JIMI uses file suffix to correctly load image
                    }
                } finally {

                    if (image == null) {

                        if ( !quiet) {
                            MipavUtil.displayError("Unable to load image. Image format may not be supported.");
                        }

                        return null;
                    }
                }
            } catch (final InterruptedException e) {

                if (image == null) {
                    return null;
                }

                Preferences.debug("FileIO.JIMI : " + e + "\n", Preferences.DEBUG_FILEIO);
            }

            mediaTracker.removeImage(image);

            imageWidth = image.getWidth(null);
            imageHeight = image.getHeight(null);

            if ( (imageWidth <= 0) || (imageHeight <= 0)) {
                return null;
            }

            // More to be added if animated gifs ... are required
            // LUT = img.getProperty("", UI.getMainFrame());
            // This is for RGB images
            final int[] pixels = new int[imageWidth * imageHeight];
            final PixelGrabber pg = new PixelGrabber(image, 0, 0, imageWidth, imageHeight, pixels, 0, imageWidth);

            try {
                pg.grabPixels();
            } catch (final InterruptedException e) {
                Preferences.debug("JIMI: Interrupted waiting for pixels!" + "\n", Preferences.DEBUG_FILEIO);

                return null;
            }

            if ( (pg.getStatus() & ImageObserver.ABORT) != 0) {
                Preferences.debug("JIMI: Image fetch aborted or errored" + "\n", Preferences.DEBUG_FILEIO);

                return null;
            }

            if ( (extents[0] != 0) && ( (extents[0] != imageWidth) || (extents[1] != imageHeight))) {
                MipavUtil
                        .displayError("Images files with similar names in the directory do not have the same X-Y dimensions.\n"
                                + "You may want to disable the Multi-file option.");

                return null;
            }

            extents[0] = imageWidth;
            extents[1] = imageHeight;

            if (buffer == null) {
                buffer = new int[4 * extents[0] * extents[1] * extents[2]];
                greyBuffer = new int[extents[0] * extents[1] * extents[2]];
            }

            int a, r, g, b, pixel;

            int i = j * 4 * extents[0] * extents[1];

            for (int y = 0; y < imageHeight; y++) {

                for (int x = 0; x < imageWidth; x++) {
                    pixel = pixels[ (y * imageWidth) + x];
                    a = (pixel >> 24) & 0xff;
                    r = (pixel >> 16) & 0xff;
                    g = (pixel >> 8) & 0xff;
                    b = (pixel) & 0xff;

                    buffer[i] = a;
                    buffer[i + 1] = r;
                    buffer[i + 2] = g;
                    buffer[i + 3] = b;
                    i += 4;
                }
            }

            image = null;
        }

        if (extents[2] == 1) {
            final int[] tmp = new int[] {extents[0], extents[1]};

            extents = new int[] {tmp[0], tmp[1]};
        }

        // need to determine if the buffer identifies a grey scale image
        boolean isGrey = false;
        int r, g, b;
        for (int i = 0, k = 0; i < buffer.length; i = i + 4, k++) {
            r = buffer[i + 1];
            g = buffer[i + 2];
            b = buffer[i + 3];
            if (r == g && g == b) {
                isGrey = true;
                greyBuffer[k] = r;
            } else {
                isGrey = false;
                greyBuffer = null;
                break;
            }
        }

        if (isGrey) {
            modelImage = new ModelImage(ModelStorageBase.UBYTE, extents, fileName);
        } else {
            modelImage = new ModelImage(ModelStorageBase.ARGB, extents, fileName);
        }

        try {
            if (isGrey) {
                modelImage.importData(0, greyBuffer, true);
            } else {
                modelImage.importData(0, buffer, true);
            }
        } catch (final IOException e) {
            Preferences.debug("FileIO.JIMI : " + e + "\n", Preferences.DEBUG_FILEIO);
            e.printStackTrace();
        }

        // get the fileInfo and make sure some fields are set (like the
        // fileDir and the fileFormat
        final FileInfoBase[] fileInfo = modelImage.getFileInfo();

        for (int j = 0; j < fileInfo.length; j++) {
            fileInfo[j].setFileDirectory(fileDir);
            fileInfo[j].setFileFormat(FileUtility.JIMI);
            modelImage.setFileInfo(fileInfo[j], j);
        }

        if (image != null) {
            image.flush();
        }

        return modelImage;
    }

    /**
     * Reads a LSM file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param secondAddress DOCUMENT ME!
     * @param one Indicates that only the named file should be read, as opposed to reading the matching files in the
     *            directory, as defined by the filetype. <code>true</code> if only want to read one image from 3D
     *            dataset.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readLSM(final String fileName, final String fileDir, final int secondAddress, final boolean one) {
        ModelImage image = null;
        FileLSM imageFile;

        try {
            imageFile = new FileLSM(fileName, fileDir, secondAddress);
            createProgressBar(imageFile, fileName, FileIO.FILE_READ);
            image = imageFile.readImage(false, one);
            LUT = imageFile.getModelLUT();
            secondImage = imageFile.getSecondImage();
        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;

    }

    /**
     * Reads a multi LSM file by first reading the headers then reading in each separate file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readLSMMulti(final String fileName, final String fileDir) {
        float[] resols;
        int[] singleExtents;
        int[] singleUnitsOfMeasure;
        int[] extents; // extent of image (!def!)
        int[] unitsOfMeasure;
        int length = 0;
        int i, j;
        FileInfoBase myFileInfo;
        FileInfoBase[] fileInfo;
        String[] fileList;
        FileLSM imageFile;
        ModelImage image = null;

        int nImages;
        final int secondAddress = 0;
        int singleDims;
        double[] timeStamp;
        double[] myTimeStamp;
        boolean[] haveTimeStamp;

        try {
            fileList = FileUtility.getFileList(fileDir, fileName, quiet); // get series of files in the chosen dir
            nImages = fileList.length;

            if (nImages == 1) {
                return readLSM(fileName, fileDir, 0, false);
            }

            imageFile = new FileLSM(fileName, fileDir, secondAddress); // read in files
            imageFile.setFileName(fileList[0]);
            imageFile.readImage(true, false);
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            
            
            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("Out of memory: " + error);
            }

            error.printStackTrace();

            return null;
        }

        myFileInfo = imageFile.getFileInfo();
        singleExtents = myFileInfo.getExtents();
        singleDims = singleExtents.length;
        singleUnitsOfMeasure = myFileInfo.getUnitsOfMeasure();
        timeStamp = new double[nImages];
        haveTimeStamp = new boolean[nImages];

        if (singleDims == 2) {
            length = singleExtents[0] * singleExtents[1];
        } else {
            length = singleExtents[0] * singleExtents[1] * singleExtents[2];
        }

        try {
            resols = new float[5];

            if (nImages > 1) {

                if (singleDims == 3) {
                    extents = new int[4];
                    extents[3] = nImages;
                    extents[2] = singleExtents[2];
                    unitsOfMeasure = new int[4];
                    unitsOfMeasure[3] = Unit.SECONDS.getLegacyNum();
                    unitsOfMeasure[2] = singleUnitsOfMeasure[2];
                } else {
                    extents = new int[3];
                    extents[2] = nImages;
                    unitsOfMeasure = new int[3];
                    unitsOfMeasure[2] = Unit.SECONDS.getLegacyNum();
                }
            } // if (nImages > 1)
            else {

                if (singleDims == 3) {
                    extents = new int[3];
                    extents[2] = singleExtents[2];
                    unitsOfMeasure = new int[3];
                    unitsOfMeasure[2] = singleUnitsOfMeasure[2];
                } else {
                    extents = new int[2];
                    unitsOfMeasure = new int[2];
                }
            }

            extents[0] = singleExtents[0]; // copy out current [0,1] coords
            extents[1] = singleExtents[1]; // so all 3 ([0,1,2]) may be added
            unitsOfMeasure[0] = singleUnitsOfMeasure[0];
            unitsOfMeasure[1] = singleUnitsOfMeasure[1];

            resols = myFileInfo.getResolutions(); // ??

            myFileInfo.setExtents(extents); //
            myFileInfo.setResolutions(resols); // ??
            image = new ModelImage(myFileInfo.getDataType(), extents, myFileInfo.getFileName());

            createProgressBar(null, FileUtility.trimNumbersAndSpecial(fileName) + FileUtility.getExtension(fileName),
                    FileIO.FILE_READ);

        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("Out of memory: " + error);
            }

            error.printStackTrace();

            return null;
        }

        image.setFileInfo(myFileInfo, 0);
        imageFile.finalize();
        imageFile = null;

        try {
            imageFile = new FileLSM(fileList[0], fileDir, secondAddress);
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            return null;
        }

        // loop through image and store data in image model
        if (singleDims == 2) {

            for (i = 0; i < nImages; i++) {

                try {
                    progressBar.setTitle(UI.getProgressBarPrefix() + "image " + fileList[i]);
                    progressBar.updateValueImmed(Math.round((float) i / (nImages - 1) * 100));
                    (imageFile).setFileName(fileList[i]);

                    // fileLSM.testme(i);
                    (imageFile).readImage(true, false);
                    myFileInfo = (imageFile).getFileInfo();
                    myFileInfo.setExtents(extents);
                    myFileInfo.setUnitsOfMeasure(unitsOfMeasure);
                    myTimeStamp = ((FileInfoLSM) myFileInfo).getTimeStamp();

                    if (myTimeStamp != null) {
                        timeStamp[i] = myTimeStamp[0];
                        haveTimeStamp[i] = true;
                    } else {
                        haveTimeStamp[i] = false;
                    }

                    myFileInfo.setResolutions(resols);

                    if (nImages > 1) {
                        myFileInfo.setMultiFile(true);
                    }

                    image.setFileInfo(myFileInfo, i);

                    // float[] tmpBuffer = fileLSM.getImageBuffer();
                    if (image.isColorImage()) {
                        image.importData(i * 4 * length, (imageFile).getImageBuffer(), false);
                    } else {
                        image.importData(i * length, (imageFile).getImageBuffer(), false);
                    }

                } catch (final IOException error) {

                    if ( !quiet) {
                        MipavUtil.displayError("FileIO: " + error);
                    }

                    error.printStackTrace();

                    if (image != null) {
                        image.disposeLocal();
                        image = null;
                    }

                    System.gc();

                    return null;
                } catch (final ArrayIndexOutOfBoundsException error) {

                    if ( !quiet) {
                        MipavUtil.displayError("Unable to read images: the image\n" + "number in the file "
                                + myFileInfo.getFileName() + " is corrupted.");
                    }

                    error.printStackTrace();

                    if (image != null) {
                        image.disposeLocal();
                        image = null;
                    }

                    System.gc();

                    return null;
                } catch (final OutOfMemoryError error) {

                    if ( !quiet) {
                        MipavUtil.displayError("Out of memory: " + error);
                    }

                    error.printStackTrace();

                    if (image != null) {

                        image.disposeLocal();
                        image = null;
                    }

                    System.gc();

                    return null;
                }
            } // for (i = 0; i < nImages; i++)
        } // if (singleDims == 2)
        else { // for singleDims == 3

            for (i = 0; i < nImages; i++) {

                try {
                    progressBar.setTitle(UI.getProgressBarPrefix() + "image " + fileList[i]);
                    progressBar.updateValueImmed(Math.round((float) i / (nImages - 1) * 100));
                    (imageFile).setFileName(fileList[i]);

                    // fileLSM.testme(i);
                    (imageFile).readImage(true, false);
                    myFileInfo = (imageFile).getFileInfo();
                    myFileInfo.setExtents(extents);
                    myFileInfo.setUnitsOfMeasure(unitsOfMeasure);
                    myTimeStamp = ((FileInfoLSM) myFileInfo).getTimeStamp();

                    if (myTimeStamp != null) {
                        timeStamp[i] = myTimeStamp[0];
                        haveTimeStamp[i] = true;
                    } else {
                        haveTimeStamp[i] = false;
                    }

                    myFileInfo.setResolutions(resols);

                    if (nImages > 1) {
                        myFileInfo.setMultiFile(true);
                    }

                    for (j = 0; j < extents[2]; j++) {
                        image.setFileInfo(myFileInfo, (i * extents[2]) + j);
                        // float[] tmpBuffer = fileLSM.getImageBuffer();

                        if (image.isColorImage()) {
                            image.importData( (i * 4 * length) + (j * 4 * extents[0] * extents[1]), (imageFile)
                                    .getImage3DMultiBuffer()[j], false);
                        } else {
                            image.importData( (i * length) + (j * extents[0] * extents[1]), (imageFile)
                                    .getImage3DMultiBuffer()[j], false);
                        }
                    } // for (j = 0; j < extents[2]; j++) {
                } catch (final IOException error) {

                    if ( !quiet) {
                        MipavUtil.displayError("FileIO: " + error);
                    }

                    error.printStackTrace();

                    if (image != null) {
                        image.disposeLocal();
                        image = null;
                    }

                    System.gc();

                    return null;
                } catch (final ArrayIndexOutOfBoundsException error) {

                    if ( !quiet) {
                        MipavUtil.displayError("Unable to read images: the image\n" + "number in the file "
                                + myFileInfo.getFileName() + " is corrupted.");
                    }

                    error.printStackTrace();

                    if (image != null) {
                        image.disposeLocal();
                        image = null;
                    }

                    System.gc();

                    return null;
                } catch (final OutOfMemoryError error) {

                    if ( !quiet) {
                        MipavUtil.displayError("Out of memory: " + error);
                    }

                    error.printStackTrace();

                    if (image != null) {

                        image.disposeLocal();
                        image = null;
                    }

                    System.gc();

                    return null;
                }
            } // for (i = 0; i < nImages; i++)
        } // else for singleDims == 3

        fileInfo = image.getFileInfo();

        if (singleDims == 2) {

            for (i = 0; i < (nImages - 1); i++) {

                if ( (haveTimeStamp[i] && haveTimeStamp[i + 1]) && (timeStamp[i] != timeStamp[i + 1])) {
                    resols = fileInfo[i].getResolutions();
                    resols[2] = (float) (timeStamp[i + 1] - timeStamp[i]);
                    fileInfo[i].setResolutions(resols);
                }
            }

            if ( (haveTimeStamp[nImages - 2] && haveTimeStamp[nImages - 1])
                    && (timeStamp[nImages - 2] != timeStamp[nImages - 1])) {
                fileInfo[nImages - 1].setResolutions(resols);
            }
        } // if (singleDims == 2)
        else { // for singleDims == 3

            for (i = 0; i < (nImages - 1); i++) {

                if ( (haveTimeStamp[i] && haveTimeStamp[i + 1]) && (timeStamp[i] != timeStamp[i + 1])) {

                    for (j = 0; j < extents[2]; j++) {
                        resols = fileInfo[ (i * extents[2]) + j].getResolutions();
                        resols[3] = (float) (timeStamp[i + 1] - timeStamp[i]);
                        fileInfo[ (i * extents[2]) + j].setResolutions(resols);
                    }
                }
            }

            if ( (haveTimeStamp[nImages - 2] && haveTimeStamp[nImages - 1])
                    && (timeStamp[nImages - 2] != timeStamp[nImages - 1])) {

                for (j = 0; j < extents[2]; j++) {
                    resols = fileInfo[ ( (nImages - 1) * extents[2]) + j].getResolutions();
                    resols[3] = (float) (timeStamp[nImages - 1] - timeStamp[nImages - 2]);
                    fileInfo[ ( (nImages - 1) * extents[2]) + j].setResolutions(resols);
                }
            }
        } // else for singleDims == 3

        imageFile.finalize();
        imageFile = null;
        return image;
    }

    /**
     * Reads multiple Magnetom Vision files by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * 
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readMagnetomVisionMulti(final String fileName, final String fileDir) {
        ModelImage image = null;
        FileMagnetomVision imageFile;
        FileInfoBase myFileInfo;

        String[] fileList;
        short[] buffer;
        int[] extents;
        int[] imageNumbers;
        int[] indicies;
        int length = 0;
        int i;
        int width, height;
        int nImages;

        try {
            fileList = FileUtility.getFileList(fileDir, fileName, quiet);
            imageFile = new FileMagnetomVision(fileName, fileDir);

            imageFile.setFileName(fileList[0]);

            createProgressBar(null, fileName, FileIO.FILE_READ);

            myFileInfo = imageFile.readHeader();
            width = myFileInfo.getExtents()[0];
            height = myFileInfo.getExtents()[1];
            length = width * height;
            buffer = new short[length];

            if (fileList.length == 1) {
                extents = new int[2];
                extents[0] = width;
                extents[1] = height;
            } else {
                extents = new int[3];
                extents[0] = width;
                extents[1] = height;
                extents[2] = fileList.length;
            }

            image = new ModelImage(ModelStorageBase.SHORT, extents, myFileInfo.getFileName());

            nImages = fileList.length;
            imageNumbers = new int[nImages];
            indicies = new int[nImages];
            progressBar.setTitle("Reading headers");

            for (i = 0; i < nImages; i++) {
                progressBar.updateValueImmed(Math.round((float) i / (nImages - 1) * 10));
                imageFile.setFileName(fileList[i]);
                myFileInfo = imageFile.readHeader();
                imageNumbers[i] = ((FileInfoMagnetomVision) myFileInfo).getTextImageNumber();
                indicies[i] = i;
            }

            FileIO.sort(imageNumbers, indicies, nImages);
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        // loop through files, place them in image array
        try {

            for (i = 0; i < nImages; i++) {
                progressBar.setTitle(UI.getProgressBarPrefix() + "image " + fileList[i]);
                progressBar.updateValueImmed(Math.round(10 + ((float) i / (nImages - 1) * 90)));

                if (fileList[i] != null) {
                    imageFile.setFileName(fileList[i]);
                    myFileInfo = imageFile.readHeader();
                    imageFile.readImage(buffer);
                    myFileInfo.setExtents(extents);

                    // myFileInfo.setStartLocations(((FileInfoGESigna5X)(myFileInfo)).getStart(imageFile.getImageNumber()-1));
                    image.setFileInfo(myFileInfo, indicies[i]);
                    image.importData(indicies[i] * length, buffer, false);
                    image.setFileInfo(myFileInfo, indicies[i]);
                }
            }

            imageFile.close();
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            return null;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            if (image != null) {

                image.disposeLocal();
                image = null;
            }

            System.gc();

            return null;
        }

        image.setImageName(imageFile.getFileInfo().getImageNameFromInfo(), false);

        imageFile.finalize();
        imageFile = null;
        return image;
    }

    /**
     * Reads a Map file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readMap(final String fileName, final String fileDir) {
        ModelImage image = null;
        FileMap imageFile;
        FileInfoBase fileInfo;
        int i;

        final JDialogRawIO mapIODialog = new JDialogRawIO(UI.getMainFrame(), "MAP");

        mapIODialog.setVisible(true);

        if (mapIODialog.isCancelled() == true) {
            return null;
        }

        try {
            fileInfo = new FileInfoImageXML(fileName, fileDir, FileUtility.RAW);
            image = new ModelImage(mapIODialog.getDataType(), mapIODialog.getExtents(), fileName);
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        fileInfo.setUnitsOfMeasure(mapIODialog.getUnitsOfMeasure());
        fileInfo.setResolutions(mapIODialog.getResolutions());
        fileInfo.setEndianess(mapIODialog.getEndianess());
        fileInfo.setExtents(mapIODialog.getExtents());

        if (fileInfo.getExtents().length > 2) { // Set file info

            for (i = 0; i < fileInfo.getExtents()[2]; i++) {
                image.setFileInfo(fileInfo, i);
            }
        } else {
            image.setFileInfo(fileInfo, 0);
        }

        try {
            imageFile = new FileMap(fileName, fileDir, fileInfo, FileBase.READ);
            createProgressBar(imageFile, fileName, FileIO.FILE_READ);
            imageFile.readImage(image, mapIODialog.getOffset());
        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        return image;

    }

    /**
     * Reads a MedVision file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readMedVision(final String fileName, final String fileDir) {
        ModelImage image = null;
        FileMedVision imageFile;

        try {
            imageFile = new FileMedVision(fileName, fileDir);
            createProgressBar(imageFile, fileName, FileIO.FILE_READ);
            image = imageFile.readImage();
        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        return image;

    }

    /**
     * Reads a MGH file by calling the read method of the file. if so, calls that method instead.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param one Indicates that only the named file should be read, as opposed to reading the matching files in the
     *            directory, as defined by the filetype. <code>true</code> if only want to read one image from 3D
     *            dataset.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readMGH(final String fileName, final String fileDir, final boolean one) {
        ModelImage image = null;
        FileMGH imageFile;

        try {
            imageFile = new FileMGH(fileName, fileDir);
            createProgressBar(imageFile, fileName, FileIO.FILE_READ);
            image = imageFile.readImage(one);
        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;
    }

    /**
     * Reads an Micro Cat file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param one Indicates that only the named file should be read, as opposed to reading the matching files in the
     *            directory, as defined by the filetype. <code>true</code> if only want to read one image from 3D
     *            dataset.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readMicroCat(final String fileName, final String fileDir, final boolean one) {
        ModelImage image = null;
        FileMicroCat imageFile;
        FileInfoMicroCat fileInfoMicro;

        try {
            imageFile = new FileMicroCat(fileName, fileDir);

            if (fileName.endsWith(".ct")) {
                int i;

                final File imageDir = new File(fileDir);
                final String[] fileList = imageDir.list();

                for (i = 0; i < fileList.length; i++) {

                    if (fileList[i].endsWith(".log")) {
                        imageFile.setFileName(fileList[i]);
                        fileInfoMicro = imageFile.readHeader();

                        if (FileMicroCat.trimmer(fileName).equals(
                                fileInfoMicro.getBaseNameforReconstructedSlices() + "_")) {
                            break;
                        }
                    }
                }

                if (i == fileList.length) {

                    if ( !quiet) {
                        MipavUtil.displayError("No appropriate header files for " + fileName);
                    }

                    return null;
                }
            }

            // at this point either image ended with .log and should read normally,
            // or appropriate header file was found and set in imageFile, and can read normally
            if (one) {
                image = imageFile.readImage(quiet, one, fileName);
            } else {
                image = imageFile.readImage(quiet);
            }
        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;
    }

    /**
     * Reads a MINC file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param one Indicates that only the named file should be read, as opposed to reading the matching files in the
     *            directory, as defined by the filetype. <code>true</code> if only want to read one image from 3D
     *            dataset.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readMinc(final String fileName, final String fileDir, final boolean one) {
        ModelImage image = null;
        FileMinc imageFile;

        try {
            imageFile = new FileMinc(fileName, fileDir);
            createProgressBar(imageFile, fileName, FileIO.FILE_READ);
            image = imageFile.readImage(one);
        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug(error + "\n", Preferences.DEBUG_FILEIO);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;
    }

    private ModelImage readMincHDF(final String fileName, final String fileDir, final boolean one) {
        ModelImage image = null;
        FileMincHDF imageFile;

        try {
            imageFile = new FileMincHDF(fileName, fileDir);
            createProgressBar(imageFile, fileName, FileIO.FILE_READ);
            image = imageFile.readImage(one);
        } catch (final Exception error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug(error + "\n", Preferences.DEBUG_FILEIO);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;
    }

    /**
     * Reads a MRC file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readMRC(final String fileName, final String fileDir) {
        ModelImage image = null;
        FileMRC imageFile;

        try {
            imageFile = new FileMRC(fileName, fileDir);
            createProgressBar(imageFile, fileName, FileIO.FILE_READ);
            image = imageFile.readImage();
            // LUT = imageFile.getModelLUT();
        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;

    }

    /**
     * Reads a BFLOAT file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param one If true, read in only one middle slice
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readBFLOAT(final String fileName, final String fileDir, final boolean one) {
        ModelImage image = null;
        FileBFLOAT imageFile;

        try {
            imageFile = new FileBFLOAT(fileName, fileDir);
            createProgressBar(imageFile, fileName, FileIO.FILE_READ);
            image = imageFile.readImage(one);
        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;

    }

    /**
     * Reads a NIFTI file by calling the read method of the file. if so, calls that method instead.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param one Indicates that only the named file should be read, as opposed to reading the matching files in the
     *            directory, as defined by the filetype. <code>true</code> if only want to read one image from 3D
     *            dataset.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readNIFTI(final String fileName, final String fileDir, final boolean one,
            final boolean niftiCompressed) {
        ModelImage image = null;
        FileNIFTI imageFile;

        try {
            imageFile = new FileNIFTI(fileName, fileDir);
            if ( !quiet) {
                createProgressBar(imageFile, fileName, FileIO.FILE_READ);
            }
            if (niftiCompressed) {
                image = imageFile.readImage(one, true);
            } else {
                image = imageFile.readImage(one, false);
            }

        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }
        imageFile.finalize();
        imageFile = null;

        return image;
    }

    /**
     * Reads a multi NIFTI file. Gets a list of the images from the file directory and reads them each in.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readNIFTIMulti(final String fileName, final String fileDir) {
        ModelImage image = null;
        FileNIFTI imageFile;
        FileInfoNIFTI fileInfo;
        TransMatrix matrix = null;
        TransMatrix matrix2 = null;

        int length = 0;
        float[] buffer;
        String[] fileList;
        int[] extents;
        float[] resolutions;
        int nImages;

        // of proper extents (in case there is a file with the consistent filename but
        // inconsistent extents.) we do assume the 1st header is correct

        int i = 0;

        try {
            fileList = FileUtility.getFileList(fileDir, fileName, quiet);

            for (final String element : fileList) {

                if (element != null) {

                    // System.out.println(" Name = " + fileList[m]);
                    i++;
                }
            }
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        nImages = i; // total number of suspected files to import into an image

        if (nImages == 1) {
            return readNIFTI(fileName, fileDir, false, false);
        }

        createProgressBar(null, FileUtility.trimNumbersAndSpecial(fileName) + FileUtility.getExtension(fileName),
                FileIO.FILE_READ);

        //          .println("nImage = " + i);
        // System.out.println(" filelist[0] = " + fileList[0]);
        // System.out.println(" filelist[1] = " + fileList[1]);
        // if one of the images has the wrong extents, the following must be changed.
        // (ei., too many images!)
        // for simplicity of setup, read in the first file hdr
        imageFile = new FileNIFTI(fileList[0], fileDir);

        try {

            if ( !imageFile.readHeader(fileList[0], fileDir, false)) {
                throw (new IOException(" NIFTI header file error"));
            }
        } catch (final IOException ioe) {

            if ( !quiet) {
                MipavUtil.displayError("Error reading header file.");
            }

            ioe.printStackTrace();
        }

        fileInfo = (imageFile).getFileInfo();
        extents = fileInfo.getExtents();
        resolutions = fileInfo.getResolutions();

        if (extents.length == 2) {
            length = extents[0] * extents[1];
        } else if (extents.length == 3) {
            length = extents[0] * extents[1] * extents[2];
        } else if (extents.length == 4) {
            length = extents[0] * extents[1] * extents[2] * extents[3];
        }

        buffer = new float[length];

        final int[] imgExtents = new int[extents.length + 1];
        final float[] imgResolutions = new float[resolutions.length + 1]; // should be same size as extents.

        // copy proper values into img extents, assuming that the 1st (numerically indexed) images
        // is correct to begin with
        for (i = 0; i < extents.length; i++) {
            imgExtents[i] = extents[i];
            imgResolutions[i] = resolutions[i];
            // set the number of slices in the image later.
        }

        imgExtents[i] = nImages; // may not be right, but we'll find out after we go through all images.
        imgResolutions[i] = 1; // resolution in the created axis is not physically defined; is generated.

        image = new ModelImage(fileInfo.getDataType(), imgExtents, fileInfo.getFileName());
        imageFile.finalize();
        imageFile = null;

        int imageCount = 0;
        int fInfoCount = 0;

        // loop through image and store data in image model
        for (i = 0; i < nImages; i++) {

            try {
                progressBar.setTitle(UI.getProgressBarPrefix() + "image " + fileList[i]);
                progressBar.updateValueImmed(Math.round((float) i / (nImages - 1) * 100));
                imageFile = new FileNIFTI(fileList[i], fileDir);

                if ( ! (imageFile).readHeader(fileList[i], fileDir, false)) {
                    throw (new IOException(" NIFTI header file error"));
                }

                // chk the extents of the image to verify it is consistent
                // (this doesn't ensure there won't be null exceptions@)
                fileInfo = (imageFile).getFileInfo();

                if (extents.length != fileInfo.getExtents().length) {

                    if ( !quiet) {
                        MipavUtil
                                .displayError("Inconsistent nifti image file found.  This File will be skipped.  The number of dimensions does not match.");
                    }

                    continue;
                } else { // the prototype image and the read-in image are of the same dimension....

                    switch (extents.length) { // check that they extend as far in all dimensions:

                        case 2:
                            if ( (extents[0] != fileInfo.getExtents()[0]) || (extents[1] != fileInfo.getExtents()[1])) {

                                if ( !quiet) {
                                    MipavUtil
                                            .displayError("Inconsistent nifti image file found.  One or more of the X-Y dimensions do not match.");
                                }

                                continue;
                            }

                            break;

                        case 3:
                            if ( (extents[0] != fileInfo.getExtents()[0]) || (extents[1] != fileInfo.getExtents()[1])
                                    || (extents[2] != fileInfo.getExtents()[2])) {

                                if ( !quiet) {
                                    MipavUtil
                                            .displayError("Inconsistent nifti image file found.  One or more of the X-Y-Z dimensions do not match.");
                                }

                                continue;
                            }

                            break;

                        case 4:
                            if ( (extents[0] != fileInfo.getExtents()[0]) || (extents[1] != fileInfo.getExtents()[1])
                                    || (extents[2] != fileInfo.getExtents()[2])
                                    || (extents[3] != fileInfo.getExtents()[3])) {

                                if ( !quiet) {
                                    MipavUtil
                                            .displayError("Inconsistent nifti image file found.  One or more of the X-Y-Z-T dimensions do not match.");
                                }

                                continue;
                            }

                            break;

                        default:
                            break;
                    }
                }

                fileInfo.setExtents(imgExtents); // set image extents to proper value!
                fileInfo.setResolutions(imgResolutions);

                if (nImages > 1) {
                    fileInfo.setMultiFile(true);
                }

                (imageFile).readImage(buffer);
                if (imageCount == 0) {
                    matrix = (imageFile).getMatrix();
                    matrix2 = (imageFile).getMatrix2();
                }
                image.importData(imageCount * length, buffer, false);

                if (image.getExtents().length > 3) {

                    for (int j = 0; j < image.getExtents()[2]; j++) {
                        image.setFileInfo(fileInfo, fInfoCount);
                        fInfoCount++;
                    }
                } else {
                    image.setFileInfo(fileInfo, imageCount);
                }

                imageCount++;

            } catch (final IOException error) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                error.printStackTrace();

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (final ArrayIndexOutOfBoundsException error) {

                if ( !quiet) {
                    MipavUtil.displayError("Unable to read images: the image\nnumber in the file "
                            + fileInfo.getFileName() + " is corrupted.");
                }

                error.printStackTrace();

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (final OutOfMemoryError error) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                error.printStackTrace();

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            }
            imageFile.finalize();
            imageFile = null;
        }
        // i goes 1 too far anyway, but if we skipped files, be sure to account for it,
        // because our basic model was that all prperly named files were good analyze images.
        // only we found one or more didn't fit. We must now take that into account.
        // ie., we read in imageCount # of images, we expected nImages.

        if (imageCount < nImages) {
            final FileInfoBase[] fileInfoArr = image.getFileInfo();

            imgExtents[image.getNDims() - 1] = imageCount; // last dimension available should be the num images read

            final int sliceSize = buffer.length;

            buffer = new float[sliceSize * imageCount];

            try {
                image.exportData(0, buffer.length, buffer); // copy all buffer
                image.reallocate(fileInfo.getDataType(), imgExtents);
                image.importData(0, buffer, true); // remake the model image with the right number of slices.

                for (i = 0; i < imgExtents[imgExtents.length - 1]; i++) { // copy all image info
                    fileInfoArr[i].setExtents(imgExtents); // update extents
                    image.setFileInfo(fileInfoArr[i], i); // copying...
                }
            } catch (final IOException ioe) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO reports: " + ioe.getMessage());
                }

                ioe.printStackTrace();

                return null;
            }

            for (i = 0; i < imgExtents[imgExtents.length - 1]; i++) { // copy all image info
                fileInfoArr[i].setExtents(imgExtents); // update extents

                // fineInfoArr[i].setN

                image.setFileInfo(fileInfoArr[i], i); // copying...
            }
        }

        image.setMatrix(matrix);
        if (matrix2 != null) {
            image.getMatrixHolder().addMatrix(matrix2);
        }

        return image;

    }

    /**
     * Reads a NRRD file by calling the read method of the file. if so, calls that method instead.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param one Indicates that only the named file should be read, as opposed to reading the matching files in the
     *            directory, as defined by the filetype. <code>true</code> if only want to read one image from 3D
     *            dataset.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readNRRD(final String fileName, final String fileDir, final boolean one) {
        ModelImage image = null;
        FileNRRD imageFile;

        try {
            imageFile = new FileNRRD(fileName, fileDir);
            createProgressBar(imageFile, fileName, FileIO.FILE_READ);
            image = imageFile.readImage(one);
        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;
    }

    /**
     * Reads in a single GE Genesis type file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * 
     * @return The image that was read in, or null if failure.
     */
    public ModelImage readGEGenesis5X(final String fileName, final String fileDir) {
        ModelImage image = null;
        FileGESigna5X imageFile;
        FileInfoBase myFileInfo;
        float[] buffer;
        int[] extents;
        int width, height;
        int imageSize = 0;

        try {
            imageFile = new FileGESigna5X(fileName, fileDir);
            imageFile.setFileName(fileName);
            imageSize = imageFile.readImageFileData();

            if (imageSize == -1) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: Compression not supported for Signa 5X");
                }

                return null;
            }

            if (imageSize == -2) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: Not a Signa 5X file.");
                }

                return null;
            }

            if (imageSize == 0) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: Image length.");
                }

                return null;
            }

            width = imageFile.getWidth();
            height = imageFile.getHeight();
            buffer = new float[width * height];

            extents = new int[2];
            extents[0] = width;
            extents[1] = height;

            image = new ModelImage(ModelStorageBase.USHORT, extents, "GE");
            imageFile.readImage(buffer);
            myFileInfo = imageFile.getFileInfo();
            myFileInfo.setExtents(extents);
            image.setFileInfo(myFileInfo, 0);
            image.setImageName(imageFile.getFileInfo().getImageNameFromInfo(), false);
            image.importData(0, buffer, false);
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;
    }

    /**
     * Reads in a single GE Signa 4x type file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * 
     * @return The image that was read in, or null if failure.
     */
    public ModelImage readGESigna4X(final String fileName, final String fileDir) {
        ModelImage image = null;
        FileGESigna4X imageFile;
        FileInfoBase myFileInfo;
        float[] buffer;
        int[] extents;
        int width, height;
        int imageSize;

        try {
            imageFile = new FileGESigna4X(fileName, fileDir);
            imageFile.setFileName(fileName);
            imageSize = imageFile.readImageFileData();

            if (imageSize == -1) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: Compression not supported for Signa 4X");
                }

                return null;
            }

            if (imageSize == -2) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: Not a Signa 4X file.");
                }

                return null;
            }

            if (imageSize == 0) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: Image length.");
                }

                return null;
            }

            width = imageFile.getWidth();
            height = imageFile.getHeight();
            buffer = new float[width * height];

            extents = new int[2];
            extents[0] = width;
            extents[1] = height;

            image = new ModelImage(ModelStorageBase.USHORT, extents, "GE");
            imageFile.readImage(buffer);
            myFileInfo = imageFile.getFileInfo();
            myFileInfo.setExtents(extents);
            image.setFileInfo(myFileInfo, 0);
            image.setImageName(imageFile.getFileInfo().getImageNameFromInfo(), false);
            image.importData(0, buffer, false);
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;
    }

    /**
     * Reads a Magnetom Vision file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readMagnetomVision(final String fileName, final String fileDir) {
        ModelImage image = null;
        FileMagnetomVision imageFile;
        FileInfoBase myFileInfo;
        int width, height;
        short[] buffer;
        int[] extents;

        try {
            imageFile = new FileMagnetomVision(fileName, fileDir);

            imageFile.setFileName(fileName);
            myFileInfo = imageFile.readHeader();
            width = myFileInfo.getExtents()[0];
            height = myFileInfo.getExtents()[1];
            buffer = new short[width * height];

            extents = new int[2];
            extents[0] = width;
            extents[1] = height;
            image = new ModelImage(ModelStorageBase.SHORT, extents, myFileInfo.getFileName());
            imageFile.readImage(buffer);
            myFileInfo.setExtents(extents);
            image.setFileInfo(myFileInfo, 0);
            image.setImageName(imageFile.getFileInfo().getImageNameFromInfo(), false);
            image.importData(0, buffer, false);
            imageFile.close();
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            return null;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            if (image != null) {

                image.disposeLocal();
                image = null;
            }

            System.gc();

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;
    }

    private ModelImage readVista(final String fileName, final String fileDir, final boolean one) {
        ModelImage image = null;
        FileVista imageFile;

        try {
            imageFile = new FileVista(fileName, fileDir);
            createProgressBar(imageFile, fileName, FileIO.FILE_READ);
            image = imageFile.readImage();
        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }
        imageFile.finalize();
        imageFile = null;
        return image;
    }

    /**
     * Reads a PARREC file by calling the read method of the file. This method contains special code to not display the
     * progress bar should the image be
     * <q>splash.img</q>.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param one Indicates that only the named file should be read, as opposed to reading the matching files in the
     *            directory, as defined by the filetype. <code>true</code> if only want to read one image from 3D
     *            dataset.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readPARREC(final String fileName, final String fileDir, final boolean one) {
        ModelImage image = null;
        FilePARREC imageFile;

        try {
            imageFile = new FilePARREC(fileName, fileDir);
            createProgressBar(imageFile, fileName, FileIO.FILE_READ);
            image = imageFile.readImage(one);
        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }
        imageFile.finalize();
        imageFile = null;
        return image;
    }

    /**
     * Reads a QT file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * 
     * @return The image that was read in, or null if failure.
     */
    @SuppressWarnings("unused")
    private ModelImage readQT(final String fileName, final String fileDir) {

        // QuickTime
        /*
         * FileQT imageFile; ModelImage image = null; try { imageFile = new FileQT(UI, fileName, fileDir); image =
         * imageFile.readImage(); //LUT = ((FileQT)imageFile).getModelLUT(); } catch (IOException error) { if (image !=
         * null) { image.disposeLocal(); image = null; } System.gc(); if (!quiet) { MipavUtil.displayError("FileIO: " +
         * error); } return null; } catch (OutOfMemoryError error) { if (image != null) { image.disposeLocal(); image =
         * null; } System.gc(); if (!quiet) { MipavUtil.displayError("FileIO: " + error); } return null; }
         */
        return null;

    }

    /**
     * Reads a RAW file by calling the read method of the file. Gets the necessary information from a dialog, if the
     * <code>fileInfo</code> parameter is null, and otherwise relies on the information stored in
     * <code>fileInfo</code> to properly read in the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param fileInfo File info of the image file; usually null, but if defined no dialog appears.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readRaw(final String fileName, final String fileDir, FileInfoBase fileInfo) {
        ModelImage image = null;
        FileRaw imageFile;
        int i;

        if (fileInfo == null) {
            fileInfo = new FileInfoImageXML(fileName, fileDir, FileUtility.RAW);

            if (rawInfo == null) {
                final JDialogRawIO rawIODialog = new JDialogRawIO(UI.getMainFrame(), "Raw");

                rawIODialog.setVisible(true);

                if (rawIODialog.isCancelled() == true) {
                    return null;
                }

                fileInfo.setDataType(rawIODialog.getDataType());
                fileInfo.setExtents(rawIODialog.getExtents());
                fileInfo.setUnitsOfMeasure(rawIODialog.getUnitsOfMeasure());
                fileInfo.setResolutions(rawIODialog.getResolutions());
                fileInfo.setEndianess(rawIODialog.getEndianess());
                fileInfo.setOffset(rawIODialog.getOffset());
            } else {
                fileInfo.setDataType(rawInfo.getDataType());
                fileInfo.setExtents(rawInfo.getExtents());
                fileInfo.setUnitsOfMeasure(rawInfo.getUnitsOfMeasure());
                fileInfo.setResolutions(rawInfo.getResolutions());
                fileInfo.setEndianess(rawInfo.getEndianess());
                fileInfo.setOffset(rawInfo.getOffset());
            }
        }

        try {
            image = new ModelImage(fileInfo.getDataType(), fileInfo.getExtents(), fileName);
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        if (fileInfo.getExtents().length > 2) { // Set file info

            for (i = 0; i < fileInfo.getExtents()[2]; i++) {
                image.setFileInfo(fileInfo, i);
            }
        } else {
            image.setFileInfo(fileInfo, 0);
        }

        try {
            imageFile = new FileRaw(fileName, fileDir, fileInfo, FileBase.READ);

            imageFile.readImage(image, fileInfo.getOffset());
        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;
    }

    /**
     * Reads a RAW file by calling the read method of the file. Gets the necessary information from a dialog, if the
     * <code>fileInfo</code> parameter is null, and otherwise relies on the information stored in
     * <code>fileInfo</code> to properly read in the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param fileInfo File info of the image file; usually null, but if defined no dialog appears.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readRawMulti(final String fileName, final String fileDir, FileInfoBase fileInfo) {
        ModelImage image = null;
        FileRaw imageFile;
        FileInfoImageXML[] nFileInfos;
        String[] fileList;
        int i;

        int nImages;

        if (fileInfo == null) {
            final JDialogRawIO rawIODialog = new JDialogRawIO(UI.getMainFrame(), "Raw");

            rawIODialog.setVisible(true);

            if (rawIODialog.isCancelled() == true) {
                return null;
            }

            fileInfo = new FileInfoImageXML(fileName, fileDir, FileUtility.RAW);
            fileInfo.setDataType(rawIODialog.getDataType());
            fileInfo.setExtents(rawIODialog.getExtents());
            fileInfo.setUnitsOfMeasure(rawIODialog.getUnitsOfMeasure());
            fileInfo.setResolutions(rawIODialog.getResolutions());
            fileInfo.setEndianess(rawIODialog.getEndianess());
            fileInfo.setOffset(rawIODialog.getOffset());
        }

        i = 0;

        try {
            fileList = FileUtility.getFileList(fileDir, fileName, quiet);
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO.readRawMulti: " + error);
            }

            error.printStackTrace();

            return null;
        }

        nImages = fileList.length; // total number of suspected files to import into an image

        // if nImages == 1 then display error and return ???

        if (nImages == 1) {
            return readRaw(fileName, fileDir, fileInfo);
        } else {
            fileInfo.setMultiFile(true);
        }

        createProgressBar(null, fileName, FileIO.FILE_READ);

        nFileInfos = new FileInfoImageXML[nImages];

        if (nImages > 1) {
            final int[] extents = new int[3];

            extents[0] = fileInfo.getExtents()[0];
            extents[1] = fileInfo.getExtents()[1];
            extents[2] = nImages;
            fileInfo.setExtents(extents);

            final float[] resols = new float[3];
            resols[0] = fileInfo.getResolution(0);
            resols[1] = fileInfo.getResolution(1);
            resols[2] = fileInfo.getResolution(2);
            fileInfo.setResolutions(resols);
        }

        final int length = fileInfo.getExtents()[0] * fileInfo.getExtents()[1];
        float[] buffer;

        try {
            buffer = new float[length];
            image = new ModelImage(fileInfo.getDataType(), fileInfo.getExtents(), fileName);
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            buffer = null;

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        // Copy all
        if (fileInfo.getExtents().length > 2) { // Set file info

            for (i = 0; i < nImages; i++) {
                nFileInfos[i] = (FileInfoImageXML) (fileInfo.clone());
            }

            image.setFileInfo(nFileInfos);
        } else {
            image.setFileInfo(fileInfo, 0);
        }

        for (int m = 0; m < nImages; m++) {

            try {
                imageFile = new FileRaw(fileList[m], fileDir, fileInfo, FileBase.READ);
                progressBar.updateValue((int) ( ((float) m / (float) nImages) * 100.0f), false);
                imageFile.readImage(buffer, fileInfo.getOffset(), fileInfo.getDataType());
                image.importData(m * length, buffer, false);
                imageFile.finalize();
                imageFile = null;
            } catch (final IOException error) {

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                buffer = null;

                System.gc();

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                error.printStackTrace();

                return null;
            } catch (final OutOfMemoryError error) {

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                buffer = null;

                System.gc();

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                error.printStackTrace();

                return null;
            }
        }

        return image;

    }

    /**
     * Reads an SPM file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param one Indicates that only the named file should be read, as opposed to reading the matching files in the
     *            directory, as defined by the filetype. <code>true</code> if only want to read one image from 3D
     *            dataset.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readSPM(final String fileName, final String fileDir, final boolean one) {
        ModelImage image = null;
        FileSPM imageFile;

        try {
            imageFile = new FileSPM(fileName, fileDir);
            createProgressBar(imageFile, fileName, FileIO.FILE_READ);
            image = imageFile.readImage(one);
        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;
    }

    /**
     * Reads a STK file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param one Indicates that only the named file should be read, as opposed to reading the matching files in the
     *            directory, as defined by the filetype. <code>true</code> if only want to read one image from 3D
     *            dataset.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readSTK(final String fileName, final String fileDir, final boolean one) {
        ModelImage image = null;
        FileSTK imageFile;

        try {
            imageFile = new FileSTK(fileName, fileDir);
            createProgressBar(imageFile, fileName, FileIO.FILE_READ);
            image = imageFile.readImage(false, one);
            LUT = imageFile.getModelLUT();
        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;

    }

    /**
     * Reads a Improvision OpenLab LIFF file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param one Indicates that only the named file should be read, as opposed to reading the matching files in the
     *            directory, as defined by the filetype. <code>true</code> if only want to read one image from 3D
     *            dataset.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readLIFF(final String fileName, final String fileDir, final boolean one) {
        ModelImage image = null;
        FileLIFF imageFile;

        try {
            imageFile = new FileLIFF(fileName, fileDir);
            createProgressBar(imageFile, fileName, FileIO.FILE_READ);
            image = imageFile.readImage(false, one);
            LUT = imageFile.getModelLUT();

        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;
    }
    
    /**
     * Reads a BMP file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param one Indicates that only the named file should be read, as opposed to reading the matching files in the
     *            directory, as defined by the filetype. <code>true</code> if only want to read one image from 3D
     *            dataset.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readBMP(final String fileName, final String fileDir, final boolean one) {
        ModelImage image = null;
        FileBMP imageFile;

        try {
            imageFile = new FileBMP(fileName, fileDir);
            createProgressBar(imageFile, fileName, FileIO.FILE_READ);
            image = imageFile.readImage(false, one);
            LUT = imageFile.getModelLUT();

        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;
    }
    
    private ModelImage readBMPMulti(final String fileName, final String fileDir, final boolean one) {
        
        
        float[] resols;
        int[] units;
        int[] extents; // extent of image (!def!)
        int length = 0;
        int i;
        FileInfoBase myFileInfo;
        String[] fileList;
        FileBMP imageFile;
        ModelImage image = null;
        int nFiles;
        
        try {
            fileList = FileUtility.getFileList(fileDir, fileName, quiet); // get series of files in the chosen dir
            nFiles = fileList.length;
            imageFile = new FileBMP(fileName, fileDir); // read in files
            imageFile.setFileName(fileList[0]);

            if (nFiles == 1) { // The multiFile flag is true but there is only one image in the
                // directory with the prefix name so read and return image as a single file.
                image = imageFile.readImage(false, false);
                LUT = imageFile.getModelLUT();
                imageFile.finalize();
                imageFile = null;

                return image;
            } else {
                imageFile.readImage(true, false);
            }
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("Out of memory: " + error);
            }

            error.printStackTrace();

            return null;
        }

        myFileInfo = imageFile.getFileInfo();

        try {
            resols = new float[5];
            units = new int[5];

            if (myFileInfo.getExtents().length == 3) {
                extents = new int[4];
                extents[2] = myFileInfo.getExtents()[2];
                extents[3] = nFiles;
                length = myFileInfo.getExtents()[0] * myFileInfo.getExtents()[1] * myFileInfo.getExtents()[2];
            } else if ( (myFileInfo.getExtents().length == 2) && (nFiles > 1)) {
                extents = new int[3];
                extents[2] = nFiles;
                length = myFileInfo.getExtents()[0] * myFileInfo.getExtents()[1];
            } else {
                extents = new int[2];
                length = myFileInfo.getExtents()[0] * myFileInfo.getExtents()[1];
            }

            extents[0] = myFileInfo.getExtents()[0]; // copy out current [0,1] coords
            extents[1] = myFileInfo.getExtents()[1]; // so all 3 ([0,1,2]) may be added

            resols[0] = myFileInfo.getResolutions()[0];
            resols[1] = myFileInfo.getResolutions()[1];
            resols[2] = 1;
            
            units[0] = myFileInfo.getUnitsOfMeasure(0);
            units[1] = myFileInfo.getUnitsOfMeasure(1);
            units[2] = myFileInfo.getUnitsOfMeasure(1);
            
            myFileInfo.setExtents(extents);

            image = new ModelImage(myFileInfo.getDataType(), extents, myFileInfo.getFileName());
            createProgressBar(null, FileUtility.trimNumbersAndSpecial(fileName) + FileUtility.getExtension(fileName),
                    FileIO.FILE_READ);

        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: Out of memory: " + error);
            }

            error.printStackTrace();

            return null;
        }

        image.setFileInfo(myFileInfo, 0);
        imageFile.finalize();
        imageFile = null;

        try {
            imageFile = new FileBMP(fileList[0], fileDir);
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            return null;
        }

        // loop through image and store data in image model
        for (i = 0; i < nFiles; i++) {

            try {
                progressBar.setTitle(UI.getProgressBarPrefix() + "image " + fileList[i]);
                progressBar.updateValueImmed(Math.round((float) i / (nFiles - 1) * 100));
                (imageFile).setFileName(fileList[i]);

                // fileTIFF.testme(i);
                (imageFile).readImage(true, false);
                myFileInfo = (imageFile).getFileInfo();
                myFileInfo.setExtents(extents);
                myFileInfo.setResolutions(resols);
                myFileInfo.setUnitsOfMeasure(units);

                if (nFiles > 1) {
                    myFileInfo.setMultiFile(true);
                }

                image.setFileInfo(myFileInfo, i);

                // float[] tmpBuffer = fileTIFF.getImageBuffer();
                if (image.isColorImage()) {
                    image.importData(i * 4 * length, (Number[]) (imageFile).getImageBuffer(), false);
                } else {
                    image.importData(i * length, (Number[]) (imageFile).getImageBuffer(), false);
                }
            } catch (final IOException error) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                error.printStackTrace();

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (final ArrayIndexOutOfBoundsException error) {

                if ( !quiet) {
                    MipavUtil.displayError("Unable to read images: the image\n" + "number in the file "
                            + myFileInfo.getFileName() + " is corrupted.");
                }

                error.printStackTrace();

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (final OutOfMemoryError error) {

                if ( !quiet) {
                    MipavUtil.displayError("Out of memory: " + error);
                }

                error.printStackTrace();

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            }
        }

        imageFile.finalize();
        imageFile = null;
        return image;
    }
    
    private ModelImage createMultifile(FileInfoBase myFileInfo, ModelImage origImage, String fileDir, String fileName, boolean quiet) {
        String[] fileList = FileUtility.getFileList(fileDir, fileName, quiet); // get series of files in the chosen dir
        int nFiles = fileList.length;
        
        if(nFiles == 1) {// The multiFile flag is true but there is only one image in the directory with the prefix name so read and return image as a single file.
            return origImage;
        }
        
        int[] newExtents = new int[origImage.getExtents().length+1];
        for(int i=0; i<origImage.getExtents().length; i++) {
            newExtents[i] = origImage.getExtents()[i];
        }
        newExtents[origImage.getExtents().length] = nFiles;
        
        ModelImage image = new ModelImage(origImage.getDataType(), newExtents, origImage.getImageFileName());
        myFileInfo.setExtents(newExtents);
        
        image.setFileInfo(myFileInfo, 0);
        
        return image;
        
    }
    
    private ModelImage convertToMultifile(ModelImage existingMultifileImage, ModelImage origImage, int[] extents, int pos) {
        return null;
    }

    /**
     * Reads a Improvision OpenLab LIFF file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param one Indicates that only the named file should be read, as opposed to reading the matching files in the
     *            directory, as defined by the filetype. <code>true</code> if only want to read one image from 3D
     *            dataset.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readMATLAB(final String fileName, final String fileDir, final boolean one) {
        ModelImage image = null;
        FileMATLAB imageFile;

        try {
            imageFile = new FileMATLAB(fileName, fileDir);
            createProgressBar(imageFile, fileName, FileIO.FILE_READ);
            image = imageFile.readImage(false, one);
            // LUT = imageFile.getModelLUT();

        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;
    }

    /**
     * Reads a Zeiss ZVI file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param one Indicates that only the named file should be read, as opposed to reading the matching files in the
     *            directory, as defined by the filetype. <code>true</code> if only want to read one image from 3D
     *            dataset.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readZVI(final String fileName, final String fileDir, final boolean one) {
        ModelImage image = null;
        FileZVI imageFile;

        try {
            imageFile = new FileZVI(fileName, fileDir);
            createProgressBar(imageFile, fileName, FileIO.FILE_READ);
            image = imageFile.readImage(false, one);
            LUT = imageFile.getModelLUT();

        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;
    }

    /**
     * Reads a TIFF file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param one Indicates that only the named file should be read, as opposed to reading the matching files in the
     *            directory, as defined by the filetype. <code>true</code> if only want to read one image from 3D
     *            dataset.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readTiff(final String fileName, final String fileDir, final boolean one) {
        ModelImage image = null;
        FileTiff imageFile;

        try {
            imageFile = new FileTiff(fileName, fileDir);
            createProgressBar(imageFile, fileName, FileIO.FILE_READ);
            image = imageFile.readImage(false, one);
            LUT = imageFile.getModelLUT();

        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;
    }

    /**
     * Reads a multi TIFF file by first reading the headers then reading in each separate file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readTiffMulti(final String fileName, final String fileDir) {
        float[] resols;
        int[] extents; // extent of image (!def!)
        int length = 0;
        int i;
        FileInfoBase myFileInfo;
        String[] fileList;
        FileTiff imageFile;
        ModelImage image = null;

        int nFiles;

        try {
            fileList = FileUtility.getFileList(fileDir, fileName, quiet); // get series of files in the chosen dir
            nFiles = fileList.length;
            imageFile = new FileTiff(fileName, fileDir); // read in files
            imageFile.setFileName(fileList[0]);

            if (nFiles == 1) { // The multiFile flag is true but there is only one image in the
                // directory with the prefix name so read and return image as a single file.
                image = imageFile.readImage(false, false);
                LUT = imageFile.getModelLUT();
                imageFile.finalize();
                imageFile = null;

                return image;
            } else {
                imageFile.readImage(true, false);
            }
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("Out of memory: " + error);
            }

            error.printStackTrace();

            return null;
        }

        myFileInfo = imageFile.getFileInfo();

        try {
            resols = new float[5];

            if (myFileInfo.getExtents().length == 3) {
                extents = new int[4];
                extents[2] = myFileInfo.getExtents()[2];
                extents[3] = nFiles;
                length = myFileInfo.getExtents()[0] * myFileInfo.getExtents()[1] * myFileInfo.getExtents()[2];
            } else if ( (myFileInfo.getExtents().length == 2) && (nFiles > 1)) {
                extents = new int[3];
                extents[2] = nFiles;
                length = myFileInfo.getExtents()[0] * myFileInfo.getExtents()[1];
            } else {
                extents = new int[2];
                length = myFileInfo.getExtents()[0] * myFileInfo.getExtents()[1];
            }

            extents[0] = myFileInfo.getExtents()[0]; // copy out current [0,1] coords
            extents[1] = myFileInfo.getExtents()[1]; // so all 3 ([0,1,2]) may be added

            resols = myFileInfo.getResolutions();
            myFileInfo.setExtents(extents);

            image = new ModelImage(myFileInfo.getDataType(), extents, myFileInfo.getFileName());
            createProgressBar(null, FileUtility.trimNumbersAndSpecial(fileName) + FileUtility.getExtension(fileName),
                    FileIO.FILE_READ);

        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: Out of memory: " + error);
            }

            error.printStackTrace();

            return null;
        }

        image.setFileInfo(myFileInfo, 0);
        imageFile.finalize();
        imageFile = null;

        try {
            imageFile = new FileTiff(fileList[0], fileDir);
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            return null;
        }

        // loop through image and store data in image model
        for (i = 0; i < nFiles; i++) {

            try {
                progressBar.setTitle(UI.getProgressBarPrefix() + "image " + fileList[i]);
                progressBar.updateValueImmed(Math.round((float) i / (nFiles - 1) * 100));
                (imageFile).setFileName(fileList[i]);

                // fileTIFF.testme(i);
                (imageFile).readImage(true, false);
                myFileInfo = (imageFile).getFileInfo();
                myFileInfo.setExtents(extents);
                myFileInfo.setResolutions(resols);

                if (nFiles > 1) {
                    myFileInfo.setMultiFile(true);
                }

                image.setFileInfo(myFileInfo, i);

                // float[] tmpBuffer = fileTIFF.getImageBuffer();
                if (image.isColorImage()) {
                    image.importData(i * 4 * length, (imageFile).getImageBuffer(), false);
                } else {
                    image.importData(i * length, (imageFile).getImageBuffer(), false);
                }
            } catch (final IOException error) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                error.printStackTrace();

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (final ArrayIndexOutOfBoundsException error) {

                if ( !quiet) {
                    MipavUtil.displayError("Unable to read images: the image\n" + "number in the file "
                            + myFileInfo.getFileName() + " is corrupted.");
                }

                error.printStackTrace();

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (final OutOfMemoryError error) {

                if ( !quiet) {
                    MipavUtil.displayError("Out of memory: " + error);
                }

                error.printStackTrace();

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            }
        }

        imageFile.finalize();
        imageFile = null;
        return image;
    }

    /**
     * Reads a TMG file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readTMG(final String fileName, final String fileDir) {
        ModelImage image = null;
        FileTMG imageFile;

        try {
            imageFile = new FileTMG(fileName, fileDir);
            createProgressBar(imageFile, fileName, FileIO.FILE_READ);
            image = imageFile.readImage();
            // LUT = imageFile.getModelLUT();
        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        imageFile.finalize();
        imageFile = null;
        return image;

    }

    /**
     * Reads an XML file by calling the read method of the file. This method contains special code to not display the
     * progress bar should it load
     * <q>splash.xml</q>.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param one Indicates that only the named file should be read, as opposed to reading the matching files in the
     *            directory, as defined by the filetype. <code>true</code> if only want to read one image from 3D
     *            dataset.
     * 
     * @return The image that was read in, or null if failure.
     */
    public ModelImage readXML(final String fileName, final String fileDir, final boolean one,
            final boolean bDisplayProgress) {
        ModelImage image = null;
        FileImageXML imageFile;
        // don't show splash screen:

        try {
            imageFile = new FileImageXML(fileName, fileDir);

            if ( ! (fileName.equals("splash.xml") || (one == true))) {
                if (bDisplayProgress) {
                    createProgressBar(imageFile, fileName, FileIO.FILE_READ);
                }
            }

            image = imageFile.readImage(one);
            LUT = imageFile.getModelLUT();
            modelRGB = imageFile.getModelRGB();
        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            // error.printStackTrace();
            progressBar.dispose();
            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();
            progressBar.dispose();
            return null;
        }

        // System.out.println(" image = " + image);
        imageFile.finalize();
        imageFile = null;
        return image;
    }

    /**
     * Reads a multi XML file. Gets a list of the images from the file directory and reads them each in.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readXMLMulti(final String fileName, final String fileDir) {
        ModelImage image = null;
        FileImageXML imageFile;
        FileInfoImageXML fileInfo;

        int length = 0;
        float[] buffer;
        String[] fileList;
        int[] extents;
        float[] resolutions;
        int nImages;

        // of proper extents (in case there is a file with the consistent filename but
        // inconsistent extents.) we do assume the 1st header is correct

        int i = 0;

        try {
            fileList = FileUtility.getFileList(fileDir, fileName, quiet);

            for (final String element : fileList) {

                if (element != null) {

                    // System.out.println(" Name = " + fileList[m]);
                    i++;
                }
            }
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        nImages = i; // total number of suspected files to import into an image

        if (nImages == 1) {
            return readXML(fileName, fileDir, false, true);
        }

        createProgressBar(null, FileUtility.trimNumbersAndSpecial(fileName) + FileUtility.getExtension(fileName),
                FileIO.FILE_READ);

        // System.out.println(" filelist[1] = " + fileList[1]);
        // if one of the images has the wrong extents, the following must be changed.
        // (ei., too many images!)
        // for simplicity of setup, read in the first file hdr
        float[][] res = null;

        imageFile = new FileImageXML(fileList[0], fileDir);

        try {
            final TalairachTransformInfo talairach = new TalairachTransformInfo();
            res = imageFile.readHeader(fileList[0], fileDir, talairach);

            if (res == null) {
                throw (new IOException(" Analyze header file error"));
            }
        } catch (final IOException ioe) {

            if ( !quiet) {
                MipavUtil.displayError("Error reading header file.");
            }

            ioe.printStackTrace();
        }

        fileInfo = (imageFile).getFileInfo();
        extents = fileInfo.getExtents();
        resolutions = fileInfo.getResolutions();

        if (extents.length == 2) {
            length = extents[0] * extents[1];
        } else if (extents.length == 3) {
            length = extents[0] * extents[1] * extents[2];
        } else if (extents.length == 4) {
            length = extents[0] * extents[1] * extents[2] * extents[3];
        }

        buffer = new float[length];

        final int[] imgExtents = new int[extents.length + 1];
        final float[] imgResolutions = new float[resolutions.length + 1]; // should be same size as extents.

        // copy proper values into img extents, assuming that the 1st (numerically indexed) images
        // is correct to begin with
        for (i = 0; i < extents.length; i++) {
            imgExtents[i] = extents[i];
            imgResolutions[i] = resolutions[i];
            // set the number of slices in the image later.
        }

        imgExtents[i] = nImages; // may not be right, but we'll find out after we go through all images.
        imgResolutions[i] = 1; // resolution in the created axis is not physically defined; is generated.

        image = new ModelImage(fileInfo.getDataType(), imgExtents, fileInfo.getFileName());

        int imageCount = 0;

        // loop through image and store data in image model
        for (i = 0; i < nImages; i++) {

            try {

                // progressBar.setTitle(UI.getProgressBarPrefix() + "image " + fileList[i]);
                progressBar.updateValueImmed(Math.round((float) i / (nImages - 1) * 100));
                imageFile.setFileName(fileList[i]);
                // imageFile = new FileImageXML(UI, fileList[i], fileDir, false);

                final TalairachTransformInfo talairach = new TalairachTransformInfo();
                res = (imageFile).readHeader(fileList[i], fileDir, talairach);

                if (res == null) {
                    throw (new IOException(" XML header file error"));
                }

                // chk the extents of the image to verify it is consistent
                // (this doesn't ensure there won't be null exceptions@)
                fileInfo = (imageFile).getFileInfo();

                if (extents.length != fileInfo.getExtents().length) {

                    if ( !quiet) {
                        MipavUtil
                                .displayError("Inconsistent xml image file found.  This file will be skipped.  The number of dimensions does not match.");
                    }

                    continue;
                } else { // the prototype image and the read-in image are of the same dimension....

                    switch (extents.length) { // check that they extend as far in all dimensions:

                        case 2:
                            if ( (extents[0] != fileInfo.getExtents()[0]) || (extents[1] != fileInfo.getExtents()[1])) {

                                if ( !quiet) {
                                    MipavUtil
                                            .displayError("Inconsistent xml image file found.  This file will be skipped.  One or more of the X-Y dimensions do not match.");
                                }

                                continue;
                            }

                            break;

                        case 3:
                            if ( (extents[0] != fileInfo.getExtents()[0]) || (extents[1] != fileInfo.getExtents()[1])
                                    || (extents[2] != fileInfo.getExtents()[2])) {

                                if ( !quiet) {
                                    MipavUtil
                                            .displayError("Inconsistent xml image file found.  This file will be skipped.  One or more of the X-Y-Z dimensions do not match.");
                                }

                                continue;
                            }

                            break;

                        case 4:
                            if ( (extents[0] != fileInfo.getExtents()[0]) || (extents[1] != fileInfo.getExtents()[1])
                                    || (extents[2] != fileInfo.getExtents()[2])
                                    || (extents[3] != fileInfo.getExtents()[3])) {

                                if ( !quiet) {
                                    MipavUtil
                                            .displayError("Inconsistent xml image file found.  This file will be skipped.  One or more of the X-Y-Z-T dimensions do not match.");
                                }

                                continue;
                            }

                            break;

                        default:
                            break;
                    }
                }

                fileInfo.setExtents(imgExtents); // set image extents to proper value!
                fileInfo.setResolutions(imgResolutions);

                if (nImages > 1) {
                    fileInfo.setMultiFile(true);
                }

                (imageFile).readImage(buffer);
                image.importData(imageCount * length, buffer, false);
                image.setFileInfo(fileInfo, imageCount);
                imageCount++; // image was okay, so count it.(can't do it before b/c of offset)

            } catch (final IOException error) {

                if ( !quiet) {
                    Preferences
                            .debug(
                                    "Failed to read XML multifile. This error can be caused by attempting to read an XML file that is not actually a multi-file.\n",
                                    Preferences.DEBUG_FILEIO);
                    MipavUtil.displayError("Failed to read XML multifile.");
                }

                error.printStackTrace();

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (final ArrayIndexOutOfBoundsException error) {

                if ( !quiet) {
                    MipavUtil.displayError("Unable to read images: the image\nnumber in the file "
                            + fileInfo.getFileName() + " is corrupted.");
                }

                error.printStackTrace();

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (final OutOfMemoryError error) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                error.printStackTrace();

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            }
        }
        // i goes 1 too far anyway, but if we skipped files, be sure to account for it,
        // because our basic model was that all prperly named files were good analyze images.
        // only we found one or more didn't fit. We must now take that into account.
        // ie., we read in imageCount # of images, we expected nImages.

        if (imageCount < nImages) {
            final FileInfoBase[] fileInfoArr = image.getFileInfo();

            imgExtents[image.getNDims() - 1] = imageCount; // last dimension available should be the num images read

            final int sliceSize = buffer.length;

            buffer = new float[sliceSize * imageCount];

            try {
                image.exportData(0, buffer.length, buffer); // copy all buffer
                image.reallocate(fileInfo.getDataType(), imgExtents);
                image.importData(0, buffer, true); // remake the model image with the right number of slices.

                for (i = 0; i < imgExtents[imgExtents.length - 1]; i++) { // copy all image info
                    fileInfoArr[i].setExtents(imgExtents); // update extents
                    image.setFileInfo(fileInfoArr[i], i); // copying...
                }
            } catch (final IOException ioe) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO reports: " + ioe.getMessage());
                }

                ioe.printStackTrace();

                return null;
            }

            for (i = 0; i < imgExtents[imgExtents.length - 1]; i++) { // copy all image info
                fileInfoArr[i].setExtents(imgExtents); // update extents

                // fineInfoArr[i].setN

                image.setFileInfo(fileInfoArr[i], i); // copying...
            }
        }

        imageFile.finalize();
        imageFile = null;
        return image;
    }

    /**
     * Writes an AFNI file to store the image.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeAfni(final ModelImage image, final FileWriteOptions options) {
        FileAfni afniFile;

        try { // Construct a new file object

            if (image.getNDims() < 3) {
                MipavUtil.displayError("Error! Image must have 3 or 4 dimensions");

                return false;
            }

            final boolean loadB = false;
            final boolean doRead = false;

            afniFile = new FileAfni(options.getFileName(), options.getFileDirectory(), loadB, doRead);
            createProgressBar(afniFile, options.getFileName(), FileIO.FILE_WRITE);
            afniFile.writeImage(image, options);
            afniFile.finalize();
            afniFile = null;
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

        return true;
    }

    /**
     * Writes an analyze file to store the image.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * @param zerofunused If true, zero funused fields
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeAnalyze(final ModelImage image, final FileWriteOptions options, boolean zerofunused) {
        /*
         * Analyze does not support unsigned short. So...if image to be saved is unsigned short, check to see if the max
         * of the image is <= max of unsigned short If that is true, then show a warning that says image will be saved
         * as short If it is false, then show an error saying that user must first convert the data type other than
         * unsigned short
         * 
         */
        if (image.getType() == ModelStorageBase.USHORT) {
            final double max = image.getMax();
            if (max <= 32767) {
                // this means it will fit in to short
                if ( !quiet) {
                    final int response = JOptionPane
                            .showConfirmDialog(
                                    UI.getMainFrame(),
                                    "Analyze 7.5 does not support Unsigned Short. However, the data will fit into Signed Short. Do you wish to proceed?",
                                    "Warning", JOptionPane.YES_NO_OPTION);
                    if (response == JOptionPane.NO_OPTION) {
                        return false;
                    }
                }
            } else {
                if ( !quiet) {
                    MipavUtil
                            .displayError("Analyze 7.5 does not support Unsigned Short. You must convert the data type to something other than Unsigned Short");
                }
                return false;
            }
        }

        FileAnalyze analyzeFile;

        try { // Construct a new file object
            analyzeFile = new FileAnalyze(options.getFileName(), options.getFileDirectory());
            analyzeFile.setZerofunused(zerofunused);
            createProgressBar(analyzeFile, options.getFileName(), FileIO.FILE_WRITE);
            analyzeFile.writeImage(image, options);
            analyzeFile.finalize();
            analyzeFile = null;
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

        return true;
    }

    /**
     * Writes an AVI file to store the image.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeAvi(final ModelImage image, final FileWriteOptions options) {

        final String fileName = options.getFileName();

        FileAvi aviFile = null;

        try {
            aviFile = new FileAvi(fileName, options.getFileDirectory());

            if (fileName.endsWith(".mov") || fileName.endsWith(".MOV")) {
                aviFile.setWriteQT(true);
            }

            aviFile.setMicroSecPerFrame(options.getMicroSecPerFrame());
            aviFile.setCompressionQuality(options.getMJPEGQuality());

            if ( !aviFile.writeImage(image, options.getImageB(), options.getLUTa(), options.getLUTb(), options
                    .getRGBTa(), options.getRGBTb(), options.getRed(), options.getGreen(), options.getBlue(), options
                    .getOpacity(), options.getAlphaBlend(), options.getPaintBitmap(), options.getAVICompression())) {
                System.err.println("AVI write cancelled");
            }

            options.disposeLocal();

        } catch (final IOException ex) {
            options.disposeLocal();

            ex.printStackTrace();

            return false;
        }

        aviFile.finalize();
        aviFile = null;
        return true;
    }

    /**
     * Writes a Freesurfer COR file to store the image.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeCOR(final ModelImage image, final FileWriteOptions options) {
        FileCOR corFile;
        int i, j;
        final float[] meterResols = new float[3];

        try { // Construct a new file object

            // Do not write COR files unless they are unsigned byte and
            // 256 cubed in dimension with all resolutions 1 millimeter.
            if (image.getType() != ModelStorageBase.UBYTE) {
                MipavUtil.displayError("Error! Data Type must be unsigned byte");

                return false;
            }

            if (image.getNDims() != 3) {
                MipavUtil.displayError("Error! Image must have three dimensions");

                return false;
            }

            if (image.getExtents()[0] != 256) {
                MipavUtil.displayError("Error! X dimension must be 256");

                return false;
            }

            if (image.getExtents()[1] != 256) {
                MipavUtil.displayError("Error! Y dimension must be 256");

                return false;
            }

            if ( (options.getEndSlice() - options.getBeginSlice() + 1) != 256) {
                MipavUtil.displayError("Error! Z dimension must be 256");

                return false;
            }

            // COR file format requires resolutions to be in meters
            // Change other units to meters
            for (i = 0; i < image.getNDims(); i++) {

                switch (Unit.getUnitFromLegacyNum(image.getFileInfo()[0].getUnitsOfMeasure(i))) {

                    case METERS:
                        meterResols[i] = image.getFileInfo()[0].getResolutions()[i];
                        break;

                    case UNKNOWN_MEASURE:
                        meterResols[i] = image.getFileInfo()[0].getResolutions()[i];
                        break;

                    case CENTIMETERS:
                        meterResols[i] = 0.01f * image.getFileInfo()[0].getResolutions()[i];
                        break;

                    case MILLIMETERS:
                        meterResols[i] = 0.001f * image.getFileInfo()[0].getResolutions()[i];
                        break;

                    case INCHES:
                        meterResols[i] = 0.0254f * image.getFileInfo()[0].getResolutions()[i];
                        break;

                    case MILS:
                        meterResols[i] = 2.54e-5f * image.getFileInfo()[0].getResolutions()[i];
                        break;

                    case MICROMETERS:
                        meterResols[i] = 1.0e-6f * image.getFileInfo()[0].getResolutions()[i];
                        break;

                    case NANOMETERS:
                        meterResols[i] = 1.0e-9f * image.getFileInfo()[0].getResolutions()[i];
                        break;

                    case ANGSTROMS:
                        meterResols[i] = 1.0e-10f * image.getFileInfo()[0].getResolutions()[i];
                        break;

                    default:
                        meterResols[i] = image.getFileInfo()[0].getResolutions()[i];

                }

                if ( (meterResols[i] > (1.0E-3 - 1.0E-7)) && (meterResols[i] < (1.0E-3 + 1.0E-7))) {
                    meterResols[i] = 1.0E-3f;
                }

                if (meterResols[i] != 1.0E-3f) {
                    if (i == 0) {
                        MipavUtil.displayError("Error! x resolution must be 1.0E-3 meter");
                    } else if (i == 1) {
                        MipavUtil.displayError("Error! y resolution must be 1.0E-3 meter");
                    } else if (i == 2) {
                        MipavUtil.displayError("Error! z resolution must be 1.0E-3 meter");
                    }
                    return false;
                }

                for (j = 0; j < image.getFileInfo().length; j++) {
                    image.getFileInfo()[j].setResolutions(meterResols[i], i);
                    image.getFileInfo()[j].setUnitsOfMeasure(Unit.METERS.getLegacyNum(), i);
                }
            }

            corFile = new FileCOR(options.getFileName(), options.getFileDirectory());
            createProgressBar(corFile, options.getFileName(), FileIO.FILE_WRITE);
            corFile.writeImage(image, options);
            corFile.finalize();
            corFile = null;
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

        return true;
    }

    /**
     * Writes a DICOM file on a slice basis. This method is specifically used in very large 3d datasets where memory
     * becomes an issue DICOM images are each written to a separate file with a different header.
     * 
     * @param image The 2D image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeDicomSlice(ModelImage image, final int sliceNumber, final FileWriteOptions options,
            FileInfoDicom fileDicom, final FileInfoBase originalFileInfo, final int originalImageDataType,
            final TransMatrix originalImageMatrix, final double originalImageMin, final double originalImageMax,
            final boolean originalIsDicom, final boolean originalIsColor) {
        int index;
        String prefix = "";
        String fileSuffix = "";
        String fileDir = null;
        String fileName = null;
        FileDicom dicomFile;
        double volMin = originalImageMin;
        double volMax = originalImageMax;
        ModelImage clonedImage = null;
        boolean didClone = false;
        String patientOrientationString = null;

        if (image.getNDims() != 2) {
            return false;
        }

        progressBar.updateValue(Math.round((float) (sliceNumber - options.getBeginSlice())
                / (options.getEndSlice() - options.getBeginSlice()) * 100), false);

        // create the directory
        fileName = options.getFileName();

        if ( /* options.isSaveAs() && */options.isSaveInSubdirectory()) {
            String baseName = null;

            // find the root of the filename (without extensions)
            final int ind = options.getFileName().lastIndexOf(".");

            if (ind > 0) {
                baseName = options.getFileName().substring(0, ind);
            } else {
                baseName = options.getFileName();
            } // there wasn't an extension

            // build directory name with the added subdirectory
            fileDir = new String(options.getFileDirectory() + baseName + File.separator);
        } else {

            // fileDir = image.getFileInfo(0).getFileDirectory();
            fileDir = options.getFileDirectory(); // options doesn't change...
        }

        // make sure fileDir exists
        final File tmpFile = new File(fileDir);

        if ( !tmpFile.exists()) {

            try {
                tmpFile.mkdirs();
                // don't reset here...may need for options to hold a root if we are saving into subdirs.
                // options.setFileDirectory(fileDir);
            } catch (final Exception e) {
                MipavUtil.displayError("Unable to create directory for DICOM file: \n" + fileDir);

                e.printStackTrace();

                return false;
            }
        }

        if (originalIsDicom) {
            fileDicom = (FileInfoDicom) image.getFileInfo(0);
            fileDicom.setFileDirectory(fileDir);

            // if this is a 'save as' file, then correct the directory name in fileInfo
            // if (options.isSaveAs()) {
            // //myFileInfo.setFileDirectory (fileDir);
            // }
            if (originalIsColor) {
                // TODO: shouldn't these tags already be set?

                fileDicom.getTagTable().setValue("0028,0100", new Short((short) 8), 2);
                fileDicom.getTagTable().setValue("0028,0101", new Short((short) 8), 2);
                fileDicom.getTagTable().setValue("0028,0102", new Short((short) 7), 2);
                fileDicom.getTagTable().setValue("0028,0002", new Short((short) 3), 2); // samples per pixel
                fileDicom.getTagTable().setValue("0028,0004", new String("RGB")); // photometric
                fileDicom.getTagTable().setValue("0028,0006", new Short((short) 0), 2); // planar Config
                fileDicom.getTagTable().setValue("0002,0010", DICOM_Constants.UID_TransferLITTLEENDIANEXPLICIT);
            }

        } else { // Non DICOM images
            // fileDicom = (FileInfoDicom) image.getFileInfo(0);
            fileDicom.setEndianess(FileBase.LITTLE_ENDIAN);
            fileDicom.setRescaleIntercept(0);
            fileDicom.setRescaleSlope(1);
            fileDicom.getTagTable().setValue("0002,0010", DICOM_Constants.UID_TransferLITTLEENDIANEXPLICIT);
            fileDicom.setVr_type(VRtype.EXPLICIT);

            final boolean isMincFloatNotPet = originalFileInfo.getFileFormat() == FileUtility.MINC
                    && originalImageDataType == ModelStorageBase.FLOAT;
            final boolean isAnalyzeFloat = originalFileInfo.getFileFormat() == FileUtility.ANALYZE
                    && originalImageDataType == ModelStorageBase.FLOAT;
            final boolean isCheshireFloat = originalFileInfo.getFileFormat() == FileUtility.CHESHIRE
                    && originalImageDataType == ModelStorageBase.FLOAT;
            final boolean isNotPet = fileDicom.getModality() != FileInfoBase.POSITRON_EMISSION_TOMOGRAPHY;
            final boolean isNIFTI = originalFileInfo.getFileFormat() == FileUtility.NIFTI;

            // necessary to save (non-pet) floating point minc/analyze/cheshire files to dicom
            if ( (isMincFloatNotPet || isAnalyzeFloat || isCheshireFloat) && isNotPet) {

                clonedImage = (ModelImage) image.clone();
                didClone = true;
                int newType;
                if (originalImageMin >= 0) {
                    newType = ModelStorageBase.USHORT;
                } else {
                    newType = ModelStorageBase.SHORT;
                }

                // in-place conversion is required so that the minc file info is retained
                final AlgorithmChangeType convertType = new AlgorithmChangeType(clonedImage, newType, originalImageMin,
                        originalImageMax, clonedImage.getMin(), clonedImage.getMax(), false);
                convertType.run();

                image = clonedImage;
            }

            if (isNIFTI) {
                patientOrientationString = ((FileInfoNIFTI) originalFileInfo).getPatientOrientationString();
                if (patientOrientationString != null) {
                    fileDicom.getTagTable().setValue("0020,0037", patientOrientationString,
                            patientOrientationString.length());
                }
            }

            if ( (image.getType() == ModelStorageBase.SHORT) || (image.getType() == ModelStorageBase.USHORT)
                    || (originalFileInfo.getDataType() == ModelStorageBase.SHORT)
                    || (originalFileInfo.getDataType() == ModelStorageBase.USHORT)) {
                fileDicom.getTagTable().setValue("0028,0100", new Short((short) 16), 2);
                fileDicom.getTagTable().setValue("0028,0101", new Short((short) 16), 2);
                fileDicom.getTagTable().setValue("0028,0102", new Short((short) 15), 2);
                fileDicom.getTagTable().setValue("0028,0002", new Short((short) 1), 2); // samples per pixel
                fileDicom.getTagTable().setValue("0028,0004", new String("MONOCHROME2"), 11); // photometric

                if ( (image.getType() == ModelStorageBase.USHORT)
                        || (originalFileInfo.getDataType() == ModelStorageBase.USHORT)) {
                    fileDicom.getTagTable().setValue("0028,0103", new Short((short) 0), 2);
                } else {
                    fileDicom.getTagTable().setValue("0028,0103", new Short((short) 1), 2);
                }
            } else if ( (image.getType() == ModelStorageBase.BYTE) || (image.getType() == ModelStorageBase.UBYTE)
                    || (originalFileInfo.getDataType() == ModelStorageBase.BYTE)
                    || (originalFileInfo.getDataType() == ModelStorageBase.UBYTE)) {
                fileDicom.getTagTable().setValue("0028,0100", new Short((short) 8), 2);
                fileDicom.getTagTable().setValue("0028,0101", new Short((short) 8), 2);
                fileDicom.getTagTable().setValue("0028,0102", new Short((short) 7), 2);
                fileDicom.getTagTable().setValue("0028,0002", new Short((short) 1), 2); // samples per pixel
                fileDicom.getTagTable().setValue("0028,0004", new String("MONOCHROME2")); // photometric

                if ( (image.getType() == ModelStorageBase.UBYTE)
                        || (originalFileInfo.getDataType() == ModelStorageBase.UBYTE)) {
                    fileDicom.getTagTable().setValue("0028,0103", new Short((short) 0), 2);
                } else {
                    fileDicom.getTagTable().setValue("0028,0103", new Short((short) 1), 2);
                }
            } else if (image.getType() == ModelStorageBase.BOOLEAN) {
                fileDicom.getTagTable().setValue("0028,0100", new Short((short) 1), 2);
                fileDicom.getTagTable().setValue("0028,0101", new Short((short) 1), 2);
                fileDicom.getTagTable().setValue("0028,0102", new Short((short) 0), 2);
                fileDicom.getTagTable().setValue("0028,0002", new Short((short) 1), 2); // samples per pixel
                fileDicom.getTagTable().setValue("0028,0004", new String("MONOCHROME2")); // photometric
            } else if ( (image.getType() == ModelStorageBase.ARGB) || (image.getType() == ModelStorageBase.ARGB_USHORT)
                    || (image.getType() == ModelStorageBase.ARGB_FLOAT)) {
                fileDicom.getTagTable().setValue("0028,0100", new Short((short) 8), 2);
                fileDicom.getTagTable().setValue("0028,0101", new Short((short) 8), 2);
                fileDicom.getTagTable().setValue("0028,0102", new Short((short) 7), 2);
                fileDicom.getTagTable().setValue("0028,0002", new Short((short) 3), 2); // samples per pixel
                fileDicom.getTagTable().setValue("0028,0004", new String("RGB")); // photometric
                fileDicom.getTagTable().setValue("0028,0006", new Short((short) 0), 2); // planar Config
            } else if ( ( (image.getType() == ModelStorageBase.FLOAT) || (originalFileInfo.getDataType() == ModelStorageBase.FLOAT)) // this
                    // is
                    // new
                    // 7/8/2008
                    && (originalFileInfo.getModality() == FileInfoBase.POSITRON_EMISSION_TOMOGRAPHY)) {
                fileDicom.getTagTable().setValue("0028,0100", new Short((short) 16), 2);
                fileDicom.getTagTable().setValue("0028,0101", new Short((short) 16), 2);
                fileDicom.getTagTable().setValue("0028,0102", new Short((short) 15), 2);
                fileDicom.getTagTable().setValue("0028,0002", new Short((short) 1), 2); // samples per pixel
                fileDicom.getTagTable().setValue("0028,0004", new String("MONOCHROME2"), 11); // photometric

                if (originalImageMin >= 0) {
                    fileDicom.getTagTable().setValue("0028,0103", new Short((short) 0), 2);
                } else {
                    fileDicom.getTagTable().setValue("0028,0103", new Short((short) 1), 2);
                }
            } else {

                if ( !quiet) {
                    MipavUtil.displayError("Saving the original image type in DICOM format is not yet supported.");

                    if (image.getType() != originalFileInfo.getDataType()) {
                        Preferences
                                .debug(
                                        "writeDicom:\tThe image file type in memory and the data type in the file info do not match.\n",
                                        Preferences.DEBUG_FILEIO);
                    }
                }

                System.gc();

                return false;
            }

            fileDicom.setDataType(image.getFileInfo(0).getDataType());

            FileDicomTag tag = null;
            Object obj = null;
            double slLoc;
            int RLIndex;
            int APIndex;
            int ISIndex;
            boolean increaseRes;

            final int originalExtentsLength = originalFileInfo.getExtents().length;

            if (originalExtentsLength > 2) {
                final double sliceResolution = originalFileInfo.getResolution(2);
                // FileInfoBase[] fBase = new FileInfoBase[originalFileInfo.getExtents()[2]];
                FileInfoBase fBase;

                TransMatrix matrix = fileDicom.getPatientOrientation();
                if (matrix != null) {
                    final TransMatrix transposeMatrix = new TransMatrix(4);
                    for (int i = 0; i < 4; i++) {
                        for (int j = 0; j < 4; j++) {
                            transposeMatrix.set(i, j, matrix.get(j, i));
                        }
                    }
                    matrix = null;
                    matrix = transposeMatrix;
                    if (isNIFTI) {
                        // If matrixQ and/or matrixS is present, use to set (0,2), (1,2),
                        // and (2,2) since last column from fileDicom.getPatientOrientation
                        // is a cross product with a sign ambiguity.
                        MatrixHolder matHolder = null;
                        TransMatrix[] matrixArray = null;
                        TransMatrix matrixQ = null;
                        TransMatrix matrixS = null;
                        matHolder = image.getMatrixHolder();

                        if (matHolder != null) {
                            matrixArray = matHolder.getNIFTICompositeMatrices();

                            if (matrixArray != null) {

                                if (matrixArray.length >= 1) {

                                    if (matrixArray[0] != null) {

                                        if (matrixArray[0].isQform()) {
                                            matrixQ = matrixArray[0];
                                        } else {
                                            matrixS = matrixArray[0];
                                        }
                                    } // if (matrixArray[0] != null)
                                } // if (matrixArray.length >= 1)

                                if (matrixArray.length >= 2) {

                                    if (matrixArray[1] != null) {

                                        if (matrixArray[1].isQform()) {
                                            matrixQ = matrixArray[1];
                                        } else {
                                            matrixS = matrixArray[1];
                                        }
                                    } // if (matrixArray[1] != null)
                                } // if (matrixArray.length >= 2)
                            } // if (matrixArray != null)
                        } // if (matHolder != null)
                        if (matrixQ != null) {
                            matrix.set(0, 2, matrixQ.get(0, 2) / image.getFileInfo(0).getResolutions()[2]);
                            matrix.set(1, 2, matrixQ.get(1, 2) / image.getFileInfo(0).getResolutions()[2]);
                            matrix.set(2, 2, matrixQ.get(2, 2) / image.getFileInfo(0).getResolutions()[2]);
                        } else if (matrixS != null) {
                            matrix.set(0, 2, matrixS.get(0, 2) / image.getFileInfo(0).getResolutions()[2]);
                            matrix.set(1, 2, matrixS.get(1, 2) / image.getFileInfo(0).getResolutions()[2]);
                            matrix.set(2, 2, matrixS.get(2, 2) / image.getFileInfo(0).getResolutions()[2]);
                        }
                    } // if (isNIFTI)
                } else {
                    matrix = originalImageMatrix;
                }

                final float[] imageOrg = originalFileInfo.getOrigin();
                final double dicomOrigin[] = new double[imageOrg.length];

                for (int k = 0; k < imageOrg.length; k++) {
                    dicomOrigin[k] = imageOrg[k];
                }

                RLIndex = 0;
                APIndex = 1;
                ISIndex = 2;
                increaseRes = true;
                for (int i = 0; i <= 2; i++) {
                    if (originalFileInfo.getAxisOrientation()[i] == FileInfoBase.ORI_R2L_TYPE) {
                        RLIndex = i;
                    } else if (originalFileInfo.getAxisOrientation()[i] == FileInfoBase.ORI_L2R_TYPE) {
                        RLIndex = i;
                        if (i == 2) {
                            increaseRes = false;
                        }
                    } else if (originalFileInfo.getAxisOrientation()[i] == FileInfoBase.ORI_A2P_TYPE) {
                        APIndex = i;
                    } else if (originalFileInfo.getAxisOrientation()[i] == FileInfoBase.ORI_P2A_TYPE) {
                        APIndex = i;
                        if (i == 2) {
                            increaseRes = false;
                        }
                    } else if (originalFileInfo.getAxisOrientation()[i] == FileInfoBase.ORI_I2S_TYPE) {
                        ISIndex = i;
                    } else if (originalFileInfo.getAxisOrientation()[i] == FileInfoBase.ORI_S2I_TYPE) {
                        ISIndex = i;
                        if (i == 2) {
                            increaseRes = false;
                        }
                    }
                }
                dicomOrigin[RLIndex] += matrix.get(0, 2) * sliceResolution * (sliceNumber - options.getBeginSlice());
                dicomOrigin[APIndex] += matrix.get(1, 2) * sliceResolution * (sliceNumber - options.getBeginSlice());
                dicomOrigin[ISIndex] += matrix.get(2, 2) * sliceResolution * (sliceNumber - options.getBeginSlice());

                slLoc = dicomOrigin[2];
                if (increaseRes) {
                    slLoc += sliceResolution * (sliceNumber - options.getBeginSlice());
                } else {
                    slLoc -= sliceResolution * (sliceNumber - options.getBeginSlice());
                }

                // see if the original dicom a minc was created from was part of a larger volume. if so, preserve the
                // instance number it had
                int baseInstanceNumber = -1;

                if (originalFileInfo.getFileFormat() == FileUtility.MINC) {
                    tag = fileDicom.getTagTable().get("0020,0013");

                    if (tag != null) {
                        obj = tag.getValue(false);
                    }

                    if (obj != null) {
                        baseInstanceNumber = Integer.parseInt( ((String) obj).trim());
                        options.setRecalculateInstanceNumber(false);
                    }
                }

                // 7/8/2008
                // this handles PET float images
                // convert type to float with short or ushort range
                if ( (originalFileInfo.getModality() == FileInfoBase.POSITRON_EMISSION_TOMOGRAPHY)
                        && ( (image.getType() == ModelStorageBase.FLOAT) || (image.getType() == ModelStorageBase.DOUBLE))) {

                    // clone the image
                    // then convert type to float with short or ushort range
                    clonedImage = (ModelImage) image.clone();
                    didClone = true;
                    int newType;
                    // double newMin, newMax;
                    newType = ModelStorageBase.FLOAT;
                    double newMin, newMax;
                    if (originalImageMin >= 0) {
                        // give USHORT range
                        newMin = 0;
                        newMax = 65535;

                    } else {
                        // give SHORT range
                        newMin = Short.MIN_VALUE;
                        newMax = Short.MAX_VALUE;
                    }
                    volMin = newMin;
                    volMax = newMax;

                    final AlgorithmChangeType convertType = new AlgorithmChangeType(clonedImage, newType,
                            originalImageMin, originalImageMax, newMin, newMax, true);
                    convertType.run();

                    image = clonedImage;

                    image.calcMinMax();
                }

                double vmin;
                double vmax;

                if (originalFileInfo.getFileFormat() == FileUtility.MINC) {
                    vmin = ((FileInfoMinc) originalFileInfo).vmin;
                    vmax = ((FileInfoMinc) originalFileInfo).vmax;
                } else {
                    vmin = volMin;
                    vmax = volMax;
                }

                double slopeDivisor = vmax - vmin;

                if (slopeDivisor == 0) {
                    slopeDivisor = 1;
                }

                fBase = (FileInfoBase) fileDicom.clone();

                // Add code to modify the slice location attribute (0020, 1041) VR = DS = decimal string
                ((FileInfoDicom) (fBase)).getTagTable().setValue("0020,1041", Double.toString(slLoc),
                        Double.toString(slLoc).length());

                final String tmpStr = new String(Float.toString((float) dicomOrigin[RLIndex]) + "\\"
                        + Float.toString((float) dicomOrigin[APIndex]) + "\\"
                        + Float.toString((float) dicomOrigin[ISIndex]));

                ((FileInfoDicom) (fBase)).getTagTable().setValue("0020,0032", tmpStr, tmpStr.length());

                if (baseInstanceNumber != -1) {
                    final String instanceStr = "" + (baseInstanceNumber + sliceNumber);
                    ((FileInfoDicom) fBase).getTagTable().setValue("0020,0013", instanceStr, instanceStr.length());
                }

                // rescaling intercepts and slopes for each slice.
                if ( (fBase.getModality() == FileInfoBase.POSITRON_EMISSION_TOMOGRAPHY)
                        && ( (originalImageDataType == ModelStorageBase.FLOAT) || (originalImageDataType == ModelStorageBase.DOUBLE))) {

                    double smin, smax; // slice min and max

                    smin = image.getMin();
                    smax = image.getMax();

                    final double slope = (smax - smin) / slopeDivisor;
                    final double intercept = smin - (slope * vmin);

                    // 7/8/2008
                    if (vmin >= 0) {
                        fBase.setDataType(ModelStorageBase.USHORT);
                    } else {
                        fBase.setDataType(ModelStorageBase.SHORT);
                    }
                    fBase.setRescaleSlope(slope);
                    fBase.setRescaleIntercept(intercept);

                    ((FileInfoDicom) fBase).getTagTable().setValue("0028,1052", "" + fBase.getRescaleIntercept());
                    ((FileInfoDicom) fBase).getTagTable().setValue("0028,1053", "" + fBase.getRescaleSlope());
                }

                image.setFileInfo(fBase, 0);

            }// end if originalImage extents > 2
            else {
                image.setFileInfo(fileDicom, 0);
            }

        }// end else non-dicom images

        if (options.isSaveAs()) {
            index = options.getFileName().indexOf(".");
            prefix = options.getFileName().substring(0, index); // Used for setting file name
            fileSuffix = options.getFileName().substring(index);
        }

        try {
            String name;

            if ( ! ( (fileDicom)).isMultiFrame()) {

                // for (i = options.getBeginSlice(); i <= options.getEndSlice(); i++) {
                // progressBar.updateValue(Math.round((float) i / (options.getEndSlice()) * 100), false);
                fileDicom = (FileInfoDicom) image.getFileInfo(0);
                fileDicom.setFileDirectory(fileDir); // need to update in case it changed

                final String s = "" + (sliceNumber + 1);

                if (options.isInstanceNumberRecalculated()) {
                    fileDicom.getTagTable().setValue("0020,0013", s, s.length());
                }

                if (options.isSaveAs()) {

                    if ( (sliceNumber < 9) && (options.getEndSlice() != options.getBeginSlice())) {
                        name = prefix + "000" + (sliceNumber + 1) + fileSuffix;
                    } else if ( (sliceNumber >= 9) && (sliceNumber < 99)
                            && (options.getEndSlice() != options.getBeginSlice())) {
                        name = prefix + "00" + (sliceNumber + 1) + fileSuffix;
                    } else if ( (sliceNumber >= 99) && (sliceNumber < 999)
                            && (options.getEndSlice() != options.getBeginSlice())) {
                        name = prefix + "0" + (sliceNumber + 1) + fileSuffix;
                    } else if (options.getEndSlice() != options.getBeginSlice()) {
                        name = prefix + (sliceNumber + 1) + fileSuffix;
                    } else {
                        name = prefix + fileSuffix;
                    }
                } else {
                    name = fileDicom.getFileName();
                }

                dicomFile = new FileDicom(name, fileDir);
                dicomFile.doStampSecondary(options.doStamp());
                final int sliceSize = image.getSliceSize();
                dicomFile.writeImage(image, 0, sliceSize, 0, false);
                // }
            } else { // its a multi frame image to be saved!!!

                // progressBar.updateValue( Math.round((float)i/(endSlice) * 100));
                dicomFile = new FileDicom(fileName, fileDir); // was (UI, fileDir, fileDir). think this fixes...

                // String s=""+(i+1);
                // myFileInfo = (FileInfoDicom)image.getFileInfo(i);
                // if (saveAs) myFileInfo.updateValue("0020,0013", s, s.length());
                dicomFile.doStampSecondary(options.doStamp());
                dicomFile.writeMultiFrameImage(image, 0, 0, 0, 0);
            }
        } catch (final IOException error) {
            // image.setFileInfo(originalFileInfos);

            error.printStackTrace();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (final OutOfMemoryError error) {
            // image.setFileInfo(originalFileInfos);

            error.printStackTrace();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

        // image.setFileInfo(originalFileInfos);
        if (didClone) {
            if (clonedImage != null) {
                clonedImage.disposeLocal();
                clonedImage = null;
            }
        }

        return true;
    }

    /**
     * Writes a DICOM file to store the image. Calls a dialog if the source isn't a DICOM image. DICOM images are each
     * written to a separate file with a different header.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeDicom(ModelImage image, final FileWriteOptions options) {

        //int i;
        int index;
        String prefix = "";
        String fileSuffix = "";
        FileInfoDicom myFileInfo = null;
        FileInfoBase[] originalFileInfos;
        FileDicom dicomFile = null;
        String fileDir = null;
        String fileName = null;
        ModelImage clonedImage = null;
        final ModelImage originalImage = image;
        boolean didClone = false;
        String patientOrientationString = null;

        // if a file is being 'saved as' a dicom file, then
        // actually save it to a subdirectory, named by the base of the FileName
        fileName = options.getFileName();

        if ( /* options.isSaveAs() && */options.isSaveInSubdirectory()) {
            String baseName = null;

            // find the root of the filename (without extensions)
            final int ind = options.getFileName().lastIndexOf(".");

            if (ind > 0) {
                baseName = options.getFileName().substring(0, ind);
            } else {
                baseName = options.getFileName();
            } // there wasn't an extension

            // build directory name with the added subdirectory
            fileDir = new String(options.getFileDirectory() + baseName + File.separator);
        } else {

            // fileDir = image.getFileInfo(0).getFileDirectory();
            fileDir = options.getFileDirectory(); // options doesn't change...
        }

        // make sure fileDir exists
        final File tmpFile = new File(fileDir);

        if ( !tmpFile.exists()) {

            try {
                tmpFile.mkdirs();
                // don't reset here...may need for options to hold a root if we are saving into subdirs.
                // options.setFileDirectory(fileDir);
            } catch (final Exception e) {
                MipavUtil.displayError("Unable to create directory for DICOM file: \n" + fileDir);

                e.printStackTrace();

                return false;
            }
        }

        originalFileInfos = (image.getFileInfo().clone());

        final int sliceSize = image.getSliceSize();

        if (image.isDicomImage()) {
            myFileInfo = (FileInfoDicom) image.getFileInfo(0);
            myFileInfo.setFileDirectory(fileDir);

            // if this is a 'save as' file, then correct the directory name in fileInfo
            // if (options.isSaveAs()) {
            // //myFileInfo.setFileDirectory (fileDir);
            // }
            if (image.isColorImage()) {
                // TODO: shouldn't these tags already be set?

                myFileInfo.getTagTable().setValue("0028,0100", new Short((short) 8), 2);
                myFileInfo.getTagTable().setValue("0028,0101", new Short((short) 8), 2);
                myFileInfo.getTagTable().setValue("0028,0102", new Short((short) 7), 2);
                myFileInfo.getTagTable().setValue("0028,0002", new Short((short) 3), 2); // samples per pixel
                myFileInfo.getTagTable().setValue("0028,0004", new String("RGB")); // photometric
                myFileInfo.getTagTable().setValue("0028,0006", new Short((short) 0), 2); // planar Config
                if (saveAsEncapJP2) {
                    myFileInfo.getTagTable().setValue("0002,0010", DICOM_Constants.UID_TransferJPEG2000LOSSLESS);
                    myFileInfo.getTagTable().setValue("0028,2110", "00");
                } else {
                    myFileInfo.getTagTable().setValue("0002,0010", DICOM_Constants.UID_TransferLITTLEENDIANEXPLICIT);
                }
            }
            
        } else { // if source image is a Non-DICOM image
            myFileInfo = new FileInfoDicom(options.getFileName(), fileDir, FileUtility.DICOM);
            final JDialogSaveDicom dialog = new JDialogSaveDicom(UI.getMainFrame(), image.getFileInfo(0), myFileInfo,
                    options.isScript());

            if (dialog.isCancelled()) {
                return false;
            }
            myFileInfo.setEndianess(FileBase.LITTLE_ENDIAN);
            myFileInfo.setRescaleIntercept(0);
            myFileInfo.setRescaleSlope(1);
            if (saveAsEncapJP2) {
                myFileInfo.getTagTable().setValue("0002,0010", DICOM_Constants.UID_TransferJPEG2000LOSSLESS);
                myFileInfo.getTagTable().setValue("0028,2110", "00");
            } else {
                myFileInfo.getTagTable().setValue("0002,0010", DICOM_Constants.UID_TransferLITTLEENDIANEXPLICIT);
            }
            myFileInfo.setVr_type(VRtype.EXPLICIT);
            final boolean isMincFloatNotPet = image.getFileInfo(0).getFileFormat() == FileUtility.MINC
                    && image.getType() == ModelStorageBase.FLOAT;
            final boolean isAnalyzeFloat = image.getFileInfo(0).getFileFormat() == FileUtility.ANALYZE
                    && image.getType() == ModelStorageBase.FLOAT;
            final boolean isCheshireFloat = image.getFileInfo(0).getFileFormat() == FileUtility.CHESHIRE
                    && image.getType() == ModelStorageBase.FLOAT;
            final boolean isNotPet = myFileInfo.getModality() != FileInfoBase.POSITRON_EMISSION_TOMOGRAPHY;
            final boolean isNIFTI = image.getFileInfo(0).getFileFormat() == FileUtility.NIFTI;

            // necessary to save (non-pet) floating point minc/analyze/cheshire files to dicom
            if ( (isMincFloatNotPet || isAnalyzeFloat || isCheshireFloat) && isNotPet) {
                clonedImage = (ModelImage) image.clone();
                didClone = true;
                int newType;
                if (clonedImage.getMin() >= 0) {
                    newType = ModelStorageBase.USHORT;
                } else {
                    newType = ModelStorageBase.SHORT;
                }
                // in-place conversion is required so that the minc file info is retained
                final AlgorithmChangeType convertType = new AlgorithmChangeType(clonedImage, newType, clonedImage
                        .getMin(), clonedImage.getMax(), clonedImage.getMin(), clonedImage.getMax(), false);
                convertType.run();

                image = clonedImage;
            }
            if (isNIFTI) {
                patientOrientationString = ((FileInfoNIFTI) image.getFileInfo(0)).getPatientOrientationString();
                if (patientOrientationString != null) {
                    myFileInfo.getTagTable().setValue("0020,0037", patientOrientationString,
                            patientOrientationString.length());
                }
            }
            if ( (image.getType() == ModelStorageBase.SHORT) || (image.getType() == ModelStorageBase.USHORT)
                    || (image.getFileInfo(0).getDataType() == ModelStorageBase.SHORT)
                    || (image.getFileInfo(0).getDataType() == ModelStorageBase.USHORT)) {
                myFileInfo.getTagTable().setValue("0028,0100", new Short((short) 16), 2);
                myFileInfo.getTagTable().setValue("0028,0101", new Short((short) 16), 2);
                myFileInfo.getTagTable().setValue("0028,0102", new Short((short) 15), 2);
                myFileInfo.getTagTable().setValue("0028,0002", new Short((short) 1), 2); // samples per pixel
                myFileInfo.getTagTable().setValue("0028,0004", new String("MONOCHROME2"), 11); // photometric

                if ( (image.getType() == ModelStorageBase.USHORT)
                        || (image.getFileInfo(0).getDataType() == ModelStorageBase.USHORT)) {
                    myFileInfo.getTagTable().setValue("0028,0103", new Short((short) 0), 2);
                } else {
                    myFileInfo.getTagTable().setValue("0028,0103", new Short((short) 1), 2);
                }
            } else if ( (image.getType() == ModelStorageBase.BYTE) || (image.getType() == ModelStorageBase.UBYTE)
                    || (image.getFileInfo(0).getDataType() == ModelStorageBase.BYTE)
                    || (image.getFileInfo(0).getDataType() == ModelStorageBase.UBYTE)) {
                myFileInfo.getTagTable().setValue("0028,0100", new Short((short) 8), 2);
                myFileInfo.getTagTable().setValue("0028,0101", new Short((short) 8), 2);
                myFileInfo.getTagTable().setValue("0028,0102", new Short((short) 7), 2);
                myFileInfo.getTagTable().setValue("0028,0002", new Short((short) 1), 2); // samples per pixel
                myFileInfo.getTagTable().setValue("0028,0004", new String("MONOCHROME2")); // photometric

                if ( (image.getType() == ModelStorageBase.UBYTE)
                        || (image.getFileInfo(0).getDataType() == ModelStorageBase.UBYTE)) {
                    myFileInfo.getTagTable().setValue("0028,0103", new Short((short) 0), 2);
                } else {
                    myFileInfo.getTagTable().setValue("0028,0103", new Short((short) 1), 2);
                }
            } else if (image.getType() == ModelStorageBase.BOOLEAN) {
                myFileInfo.getTagTable().setValue("0028,0100", new Short((short) 1), 2);
                myFileInfo.getTagTable().setValue("0028,0101", new Short((short) 1), 2);
                myFileInfo.getTagTable().setValue("0028,0102", new Short((short) 0), 2);
                myFileInfo.getTagTable().setValue("0028,0002", new Short((short) 1), 2); // samples per pixel
                myFileInfo.getTagTable().setValue("0028,0004", new String("MONOCHROME2")); // photometric
            } else if (image.isColorImage()) {
                myFileInfo.getTagTable().setValue("0028,0100", new Short((short) 8), 2);
                myFileInfo.getTagTable().setValue("0028,0101", new Short((short) 8), 2);
                myFileInfo.getTagTable().setValue("0028,0102", new Short((short) 7), 2);
                myFileInfo.getTagTable().setValue("0028,0002", new Short((short) 3), 2); // samples per pixel
                myFileInfo.getTagTable().setValue("0028,0004", new String("RGB")); // photometric
                myFileInfo.getTagTable().setValue("0028,0006", new Short((short) 0), 2); // planar Config
            } else if ( ( (image.getType() == ModelStorageBase.FLOAT) || (image.getFileInfo(0).getDataType() == ModelStorageBase.FLOAT)) // 7/8/2008
                    && (myFileInfo.getModality() == FileInfoBase.POSITRON_EMISSION_TOMOGRAPHY)) {
                myFileInfo.getTagTable().setValue("0028,0100", new Short((short) 16), 2);
                myFileInfo.getTagTable().setValue("0028,0101", new Short((short) 16), 2);
                myFileInfo.getTagTable().setValue("0028,0102", new Short((short) 15), 2);
                myFileInfo.getTagTable().setValue("0028,0002", new Short((short) 1), 2); // samples per pixel
                myFileInfo.getTagTable().setValue("0028,0004", new String("MONOCHROME2"), 11); // photometric

                if (image.getMin() >= 0) {
                    myFileInfo.getTagTable().setValue("0028,0103", new Short((short) 0), 2);
                } else {
                    myFileInfo.getTagTable().setValue("0028,0103", new Short((short) 1), 2);
                }
            } else {

                if ( !quiet) {
                    MipavUtil.displayError("Saving the original image type in DICOM format is not yet supported.");

                    if (image.getType() != image.getFileInfo(0).getDataType()) {
                        Preferences
                                .debug(
                                        "writeDicom:\tThe image file type in memory and the data type in the file info do not match.\n",
                                        Preferences.DEBUG_FILEIO);
                    }
                }

                System.gc();

                return false;
            }

            myFileInfo.setDataType(image.getFileInfo(0).getDataType());

            FileDicomTag tag = null;
            Object obj = null;
            double slLoc;
            int RLIndex;
            int APIndex;
            int ISIndex;
            boolean increaseRes;
            final double sliceResolution = myFileInfo.getResolution(2);
            if (image.getNDims() > 2) {
                //create as many file-infos as dicom frames exist in the image
                int fBaseLength = image.getExtents()[2];
                for(int i=3; i<image.getExtents().length; i++) {
                    fBaseLength *= image.getExtents()[i];
                }
                final FileInfoBase[] fBase = new FileInfoBase[fBaseLength];

                TransMatrix matrix = myFileInfo.getPatientOrientation();
                if (matrix != null) {
                    final TransMatrix transposeMatrix = new TransMatrix(4);
                    for (int i = 0; i < 4; i++) {
                        for (int j = 0; j < 4; j++) {
                            transposeMatrix.set(i, j, matrix.get(j, i));
                        }
                    }
                    matrix = null;
                    matrix = transposeMatrix;
                    if (isNIFTI) {
                        // If matrixQ and/or matrixS is present, use to set (0,2), (1,2),
                        // and (2,2) since last column from fileDicom.getPatientOrientation
                        // is a cross product with a sign ambiguity.
                        MatrixHolder matHolder = null;
                        TransMatrix[] matrixArray = null;
                        TransMatrix matrixQ = null;
                        TransMatrix matrixS = null;
                        matHolder = image.getMatrixHolder();

                        if (matHolder != null) {
                            matrixArray = matHolder.getNIFTICompositeMatrices();

                            if (matrixArray != null) {

                                if (matrixArray.length >= 1) {

                                    if (matrixArray[0] != null) {

                                        if (matrixArray[0].isQform()) {
                                            matrixQ = matrixArray[0];
                                        } else {
                                            matrixS = matrixArray[0];
                                        }
                                    } // if (matrixArray[0] != null)
                                } // if (matrixArray.length >= 1)

                                if (matrixArray.length >= 2) {

                                    if (matrixArray[1] != null) {

                                        if (matrixArray[1].isQform()) {
                                            matrixQ = matrixArray[1];
                                        } else {
                                            matrixS = matrixArray[1];
                                        }
                                    } // if (matrixArray[1] != null)
                                } // if (matrixArray.length >= 2)
                            } // if (matrixArray != null)
                        } // if (matHolder != null)
                        if (matrixQ != null) {
                            matrix.set(0, 2, matrixQ.get(0, 2) / image.getFileInfo(0).getResolutions()[2]);
                            matrix.set(1, 2, matrixQ.get(1, 2) / image.getFileInfo(0).getResolutions()[2]);
                            matrix.set(2, 2, matrixQ.get(2, 2) / image.getFileInfo(0).getResolutions()[2]);
                        } else if (matrixS != null) {
                            matrix.set(0, 2, matrixS.get(0, 2) / image.getFileInfo(0).getResolutions()[2]);
                            matrix.set(1, 2, matrixS.get(1, 2) / image.getFileInfo(0).getResolutions()[2]);
                            matrix.set(2, 2, matrixS.get(2, 2) / image.getFileInfo(0).getResolutions()[2]);
                        }
                    } // if (isNIFTI)
                } else {
                    matrix = image.getMatrix();
                }


                final float[] imageOrg = image.getFileInfo(0).getOrigin();
                final double dicomOrigin[] = new double[imageOrg.length];

                for (int k = 0; k < imageOrg.length; k++) {
                    dicomOrigin[k] = imageOrg[k];
                }

                RLIndex = 0;
                APIndex = 1;
                ISIndex = 2;
                increaseRes = true;
                for (int i = 0; i <= 2; i++) {
                    if (image.getFileInfo()[0].getAxisOrientation()[i] == FileInfoBase.ORI_R2L_TYPE) {
                        RLIndex = i;
                    } else if (image.getFileInfo()[0].getAxisOrientation()[i] == FileInfoBase.ORI_L2R_TYPE) {
                        RLIndex = i;
                        if (i == 2) {
                            increaseRes = false;
                        }
                    } else if (image.getFileInfo()[0].getAxisOrientation()[i] == FileInfoBase.ORI_A2P_TYPE) {
                        APIndex = i;
                    } else if (image.getFileInfo()[0].getAxisOrientation()[i] == FileInfoBase.ORI_P2A_TYPE) {
                        APIndex = i;
                        if (i == 2) {
                            increaseRes = false;
                        }
                    } else if (image.getFileInfo()[0].getAxisOrientation()[i] == FileInfoBase.ORI_I2S_TYPE) {
                        ISIndex = i;
                    } else if (image.getFileInfo()[0].getAxisOrientation()[i] == FileInfoBase.ORI_S2I_TYPE) {
                        ISIndex = i;
                        if (i == 2) {
                            increaseRes = false;
                        }
                    }
                }

                slLoc = dicomOrigin[2];

                // see if the original dicom a minc was created from was part of a larger volume. if so, preserve the
                // instance number it had
                int baseInstanceNumber = -1;

                if (image.getFileInfo(0).getFileFormat() == FileUtility.MINC) {
                    tag = myFileInfo.getTagTable().get("0020,0013");

                    if (tag != null) {
                        obj = tag.getValue(false);
                    }

                    if (obj != null) {
                        baseInstanceNumber = Integer.parseInt( ((String) obj).trim());
                        options.setRecalculateInstanceNumber(false);
                    }
                }

                // 7/8/2008
                // this handles PET float images
                // convert type to float with short or ushort range
                if ( (myFileInfo.getModality() == FileInfoBase.POSITRON_EMISSION_TOMOGRAPHY)
                        && ( (image.getType() == ModelStorageBase.FLOAT) || (image.getType() == ModelStorageBase.DOUBLE))) {

                    // clone the image
                    // then convert type to float with short or ushort range
                    clonedImage = (ModelImage) image.clone();
                    didClone = true;

                    int newType;
                    double newMin, newMax;
                    newType = ModelStorageBase.FLOAT;

                    if (image.getMin() >= 0) {
                        // give USHORT range
                        newMin = 0;
                        newMax = 65535;

                    } else {
                        // give SHORT range
                        newMin = Short.MIN_VALUE;
                        newMax = Short.MAX_VALUE;
                    }

                    final AlgorithmChangeType convertType = new AlgorithmChangeType(clonedImage, newType, clonedImage
                            .getMin(), clonedImage.getMax(), newMin, newMax, false);
                    convertType.run();

                    clonedImage.calcMinMax();

                    image = clonedImage;

                }

                double vmin;
                double vmax;

                if (image.getFileInfo(0).getFileFormat() == FileUtility.MINC) {
                    vmin = ((FileInfoMinc) image.getFileInfo(0)).vmin;
                    vmax = ((FileInfoMinc) image.getFileInfo(0)).vmax;
                } else {
                    vmin = image.getMin();
                    vmax = image.getMax();
                }

                double slopeDivisor = vmax - vmin;

                if (slopeDivisor == 0) {
                    slopeDivisor = 1;
                }

                final float[] sliceData = new float[sliceSize];
                for (int k = 0; k < fBaseLength; k++) {

                    // System.err.println("FileIO k = " + k);
                    fBase[k] = (FileInfoBase) myFileInfo.clone();

                    // Add code to modify the slice location attribute (0020, 1041) VR = DS = decimal string
                    ((FileInfoDicom) (fBase[k])).getTagTable().setValue("0020,1041", Double.toString(slLoc),
                            Double.toString(slLoc).length());

                    if (increaseRes) {
                        slLoc += sliceResolution;
                    } else {
                        slLoc -= sliceResolution;
                    }

                    final String tmpStr = new String(Float.toString((float) dicomOrigin[RLIndex]) + "\\"
                            + Float.toString((float) dicomOrigin[APIndex]) + "\\"
                            + Float.toString((float) dicomOrigin[ISIndex]));

                    ((FileInfoDicom) (fBase[k])).getTagTable().setValue("0020,0032", tmpStr, tmpStr.length());

                    dicomOrigin[RLIndex] += matrix.get(0, 2) * sliceResolution;
                    dicomOrigin[APIndex] += matrix.get(1, 2) * sliceResolution;
                    dicomOrigin[ISIndex] += matrix.get(2, 2) * sliceResolution;

                    if (baseInstanceNumber != -1) {
                        final String instanceStr = "" + (baseInstanceNumber + k);
                        ((FileInfoDicom) (fBase[k])).getTagTable().setValue("0020,0013", instanceStr,
                                instanceStr.length());
                    }

                    // rescaling intercepts and slopes for each slice.
                    if ( (fBase[k].getModality() == FileInfoBase.POSITRON_EMISSION_TOMOGRAPHY)
                            && ( (image.getType() == ModelStorageBase.FLOAT) || (image.getType() == ModelStorageBase.DOUBLE))) {

                        double smin, smax; // slice min and max

                        try {
                            image.exportData(k * sliceSize, sliceSize, sliceData);
                        } catch (final IOException ioe) {
                            if (didClone) {
                                if (clonedImage != null) {
                                    clonedImage.disposeLocal();
                                    clonedImage = null;
                                }
                            } else {
                                originalImage.setFileInfo(originalFileInfos);
                            }

                            ioe.printStackTrace();

                            if ( !quiet) {
                                MipavUtil.displayError("FileIO: " + ioe);
                            }

                            ioe.printStackTrace();

                            return false;
                        }

                        smin = Double.MAX_VALUE;
                        smax = -Double.MAX_VALUE;

                        // calculate min max values per slice
                        for (final float element : sliceData) {

                            if (element < smin) {
                                smin = element;
                            }

                            if (element > smax) {
                                smax = element;
                            }
                        }

                        final double slope = (smax - smin) / slopeDivisor;
                        final double intercept = smin - (slope * vmin);

                        // 7/8/2008
                        if (vmin >= 0) {
                            fBase[k].setDataType(ModelStorageBase.USHORT);
                        } else {
                            fBase[k].setDataType(ModelStorageBase.SHORT);
                        }
                        fBase[k].setRescaleSlope(slope);
                        fBase[k].setRescaleIntercept(intercept);

                        ((FileInfoDicom) fBase[k]).getTagTable().setValue("0028,1052",
                                "" + fBase[k].getRescaleIntercept());
                        ((FileInfoDicom) fBase[k]).getTagTable().setValue("0028,1053", "" + fBase[k].getRescaleSlope());
                    }
                }
                image.setFileInfo(fBase);
            } else {
                image.setFileInfo(myFileInfo, 0);
            }
        }
        
        
        if(options.doEnhanced()) {
            int tDim = image.getExtents().length <= 3 ? 1 : image.getExtents()[3];
            int zDim = image.getExtents().length <= 2 ? 1 : image.getExtents()[2];
            FileInfoDicom[][] infoAr = new FileInfoDicom[zDim][tDim];
            for(int t = 0; t < tDim; t++) {
                for(int z = 0; z < zDim; z++) {
                    infoAr[z][t] = ((FileInfoDicom)image.getFileInfo(zDim*t + z));
                }
            }
            insertEnhancedSequence(myFileInfo, infoAr);
        } else {
            if(image.getFileInfo()[0] instanceof FileInfoDicom) {
                removeEnhancedDicomInfo(myFileInfo, image.getFileInfo());
            }
            myFileInfo.setMultiFrame(false);
        }
        
        image.setFileInfo(myFileInfo, 0);

        createProgressBar(null, options.getFileName(), FileIO.FILE_WRITE);

        if (options.isSaveAs()) {
            index = options.getFileName().indexOf(".");
            prefix = options.getFileName().substring(0, index); // Used for setting file name
            fileSuffix = options.getFileName().substring(index);

        }

        try {
            String name = "";
            if ( !myFileInfo.isMultiFrame() && !options.doEnhanced()) {
                final String sopUID = ((String) ((FileInfoDicom) image.getFileInfo(0)).getTagTable().get("0008,0018")
                        .getValue(true)).toString();
                for (int i = options.getBeginSlice(); i <= options.getEndSlice(); i++) {
                    progressBar.updateValue(Math.round((float) i / (options.getEndSlice()) * 100), false);

                    myFileInfo = (FileInfoDicom) image.getFileInfo(i);
                    myFileInfo.setFileDirectory(fileDir); // need to update in case it changed

                    final String s = "" + (i + 1);

                    if (options.isInstanceNumberRecalculated()) {
                        myFileInfo.getTagTable().setValue("0020,0013", s, s.length());
                    }

                    myFileInfo.getTagTable().setValue("0008,0018", sopUID + "." + i);
                    if(myFileInfo.getTagTable().getValue("0002,0003") != null) {
                        myFileInfo.getTagTable().setValue("0002,0003", sopUID + "." + i);
                    }
                    
                    if (options.isSaveAs()) {

                        if ( (i < 9) && (options.getEndSlice() != options.getBeginSlice())) {
                            name = prefix + "000" + (i + 1) + fileSuffix;
                        } else if ( (i >= 9) && (i < 99) && (options.getEndSlice() != options.getBeginSlice())) {
                            name = prefix + "00" + (i + 1) + fileSuffix;
                        } else if ( (i >= 99) && (i < 999) && (options.getEndSlice() != options.getBeginSlice())) {
                            name = prefix + "0" + (i + 1) + fileSuffix;
                        } else if (options.getEndSlice() != options.getBeginSlice()) {
                            name = prefix + (i + 1) + fileSuffix;
                        } else {
                            name = prefix + fileSuffix;
                        }
                    } else {
                        name = myFileInfo.getFileName();
                    }

                    dicomFile = new FileDicom(name, fileDir);
                    dicomFile.doStampSecondary(options.doStamp());
                    dicomFile.writeImage(image, i * sliceSize, (i * sliceSize) + sliceSize, i, saveAsEncapJP2);

                }

            } else { // its a multi frame image

                // progressBar.updateValue( Math.round((float)i/(endSlice) * 100));
                dicomFile = new FileDicom(fileName, fileDir); // was (UI, fileDir, fileDir). think this fixes...

                // String s=""+(i+1);
                // myFileInfo = (FileInfoDicom)image.getFileInfo(i);
                // if (saveAs) myFileInfo.updateValue("0020,0013", s, s.length());
                dicomFile.doStampSecondary(options.doStamp());
                dicomFile.doEnhanced(options.doEnhanced(), image.getExtents());
                dicomFile.writeMultiFrameImage(image, options.getBeginSlice(), options.getEndSlice(), options.getBeginTime(), options.getEndTime());
            }
        } catch (final IOException error) {
            if (didClone) {
                if (clonedImage != null) {
                    clonedImage.disposeLocal();
                    clonedImage = null;
                }
            } else {
                originalImage.setFileInfo(originalFileInfos);
            }

            error.printStackTrace();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (final OutOfMemoryError error) {
            if (didClone) {
                if (clonedImage != null) {
                    clonedImage.disposeLocal();
                    clonedImage = null;
                }
            } else {
                originalImage.setFileInfo(originalFileInfos);
            }

            error.printStackTrace();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

        if (didClone) {
            if (clonedImage != null) {
                clonedImage.disposeLocal();
                clonedImage = null;
            }
        } else {
            originalImage.setFileInfo(originalFileInfos);
        }

        /*
         * if(dicomFile != null) { dicomFile.finalize(); dicomFile = null; }
         */
        return true;
    }

    private void removeEnhancedDicomInfo(FileInfoDicom myFileInfo, FileInfoBase[] fileInfoBases) {
        for(int i = 0; i < fileInfoBases.length; i++) {
            ((FileInfoDicom)fileInfoBases[i]).getTagTable().removeTag("0028,0008");
            ((FileInfoDicom)fileInfoBases[i]).getTagTable().removeTag("5200,9230");
            if(((FileInfoDicom)fileInfoBases[i]).getTagTable().getReferenceTagTable() != null) {
                ((FileInfoDicom)fileInfoBases[i]).getTagTable().getReferenceTagTable().removeTag("0028,0008");
                ((FileInfoDicom)fileInfoBases[i]).getTagTable().getReferenceTagTable().removeTag("5200,9230");
            }
        }
        myFileInfo.getTagTable().removeTag("0028,0008");
        myFileInfo.getTagTable().removeTag("5200,9230");
        if(myFileInfo.getTagTable().getReferenceTagTable() != null) {
            myFileInfo.getTagTable().getReferenceTagTable().removeTag("0028,0008");
            myFileInfo.getTagTable().getReferenceTagTable().removeTag("5200,9230");
        }
    }

    private void insertEnhancedSequence(FileInfoDicom myFileInfo,
            FileInfoDicom[][] infoAr) {
        long time = System.currentTimeMillis();
        System.out.println("Oute");
        //create sequence ordered by current slice number
        FileDicomSQ seqBase = new FileDicomSQ(); //this is the 5200,9230 sequence
        seqBase.setWriteAsUnknownLength(true); //sequences containing enhanced dicom data always given known length
        
        int tDim = infoAr[0].length;
        int zDim = infoAr.length;
        for(int t = 0; t < tDim; t++) {
            for(int z = 0; z < zDim; z++) {
                FileDicomTagTable table = infoAr[z][t].getTagTable();
                
                FileDicomSQ seq = new FileDicomSQ();  //this is the 0020,9111 sequence
                FileDicomTag tag = null;
                FileDicomSQItem item = null;
                if((tag = table.get("0020,9111")) != null && !table.isTagSameAsReferenceTag(tag)) {
                    seq = (FileDicomSQ)table.get("0020,9111").getValue(false);
                    item = ((FileDicomSQ)table.get("0020,9111").getValue(false)).getItem(0);
                } else {
                    item = new FileDicomSQItem(null, myFileInfo.getVr_type());
                    seq.addItem(item);
                    seq.setWriteAsUnknownLength(true); 
                    table.setValue("0020,9111", seq, -1);
                }
                
                item.setValue("0020,9056", t+1);  //is one-based
                item.setValue("0020,9057", z+1);  //is one-based
                
                item.setWriteAsUnknownLength(false); //enhanced sequence items are always written using known length
                
                
                Enumeration<FileDicomTag> tags = table.getTagList().elements();
                Object tagValue = null;
                FileDicomSQItem outerItem = new FileDicomSQItem(null, myFileInfo.getVr_type());
                while(tags.hasMoreElements()) {
                    tag = tags.nextElement();
                    if(table == myFileInfo.getTagTable() || //if table is pointing to the same location as myFileInfo, write all tags 
                            (tagValue = myFileInfo.getTagTable().get(tag.getKey())) == null || !tag.equals(tagValue)) {
                        outerItem.setValue(tag.getKey(), tag, tag.getValue(false), -1);
                    }
                }
                seqBase.addItem(outerItem); //is now the 2D item within the sequence
                outerItem.setWriteAsUnknownLength(false);
            }
        }
        System.out.println("Finished enhanced sequence construction in "+(System.currentTimeMillis() - time));
        //insert constructed sequence into tag table
        myFileInfo.getTagTable().setValue("0028,0008", tDim*zDim);
        myFileInfo.getTagTable().setValue("5200,9230", seqBase);
    }

    /**
     * Writes a Fits file to store the image.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeFits(final ModelImage image, final FileWriteOptions options) {
        FileFits fitsFile;

        try { // Construct a new file object
            fitsFile = new FileFits(options.getFileName(), options.getFileDirectory());
            createProgressBar(fitsFile, options.getFileName(), FileIO.FILE_WRITE);
            fitsFile.writeImage(image, options);
            fitsFile.finalize();
            fitsFile = null;
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

        return true;
    }

    /**
     * Writes a GESigna4X file to store the image.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeGESigna4X(final ModelImage image, final FileWriteOptions options) {
        FileGESigna4X geFile;

        try { // Construct a new file object
            geFile = new FileGESigna4X(options.getFileName(), options.getFileDirectory());
            createProgressBar(geFile, options.getFileName(), FileIO.FILE_WRITE);
            geFile.writeImage(image, options);
            geFile.finalize();
            geFile = null;
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

        return true;
    }

    /**
     * Writes a GESigna5X file to store the image.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeGEGenesis5X(final ModelImage image, final FileWriteOptions options) {
        FileGESigna5X geFile;

        try { // Construct a new file object
            geFile = new FileGESigna5X(options.getFileName(), options.getFileDirectory());
            createProgressBar(geFile, options.getFileName(), FileIO.FILE_WRITE);
            geFile.writeImage(image, options);
            geFile.finalize();
            geFile = null;
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

        return true;
    }

    /**
     * Writes an ICS file to store the image.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeICS(final ModelImage image, final FileWriteOptions options) {
        FileICS ICSFile;

        try { // Construct a new file object

            ICSFile = new FileICS(options.getFileName(), options.getFileDirectory());
            createProgressBar(ICSFile, options.getFileName(), FileIO.FILE_WRITE);
            ICSFile.writeImage(image, options);
            ICSFile.finalize();
            ICSFile = null;
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

        return true;
    }

    /**
     * Writes an Interfile file to store the image.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeInterfile(final ModelImage image, final FileWriteOptions options) {
        FileInterfile interfileFile;

        try { // Construct a new file object
            interfileFile = new FileInterfile(options.getFileName(), options.getFileDirectory());
            createProgressBar(interfileFile, options.getFileName(), FileIO.FILE_WRITE);
            interfileFile.writeImage(image, options);
            interfileFile.finalize();
            interfileFile = null;
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

        return true;
    }

    /**
     * Writes a JIMI file to store the image.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeJimi(final ModelImage image, final FileWriteOptions options) {
        final int index = options.getFileName().indexOf(".");
        final String prefix = options.getFileName().substring(0, index); // Used for setting file name
        final String fileSuffix = options.getFileName().substring(index);
        int slice = 0;
        final boolean isVis = UI.isAppFrameVisible();
        try {
            slice = ((ViewJFrameImage) (image.getImageFrameVector().firstElement())).getComponentImage().getSlice();
        } catch (final NoSuchElementException e) {
            // put image in frame..set to invisible
            UI.setAppFrameVisible(false);
            new ViewJFrameImage(image);
            slice = ((ViewJFrameImage) (image.getImageFrameVector().firstElement())).getComponentImage().getSlice();
        }

        String name;

        final int beginSlice = options.getBeginSlice();
        final int endSlice = options.getEndSlice();

        for (int i = beginSlice; i <= endSlice; i++) {

            Image im;

            ((ViewJFrameImage) (image.getImageFrameVector().firstElement())).getComponentImage().createImg(i);

            im = ((ViewJFrameImage) (image.getImageFrameVector().firstElement())).getComponentImage().getImage();

            if ( (i < 9) && (endSlice != beginSlice)) {
                name = options.getFileDirectory() + prefix + "00" + (i + 1) + fileSuffix;
            } else if ( (i >= 9) && (i < 99) && (endSlice != beginSlice)) {
                name = options.getFileDirectory() + prefix + "0" + (i + 1) + fileSuffix;
            } else if (endSlice != beginSlice) {
                name = options.getFileDirectory() + prefix + (i + 1) + fileSuffix;
            } else {
                name = options.getFileDirectory() + prefix + fileSuffix;
            }

            try {
                Jimi.putImage(im, name);
            } catch (final JimiException jimiException) {
                Preferences.debug("JIMI write error: " + jimiException + "\n", Preferences.DEBUG_FILEIO);

                jimiException.printStackTrace();

                return false;
            }
        }

        ((ViewJFrameImage) (image.getImageFrameVector().firstElement())).getComponentImage().show(0, slice, null, null,
                true, -1);

        UI.setAppFrameVisible(isVis);

        return true;
    }

    /**
     * Writes a MGH or MGZ file to store the image.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeMGH(final ModelImage image, final FileWriteOptions options) {
        FileMGH mghFile;

        try { // Construct a new file object
            mghFile = new FileMGH(options.getFileName(), options.getFileDirectory());
            createProgressBar(mghFile, options.getFileName(), FileIO.FILE_WRITE);
            mghFile.writeImage(image, options);
            mghFile.finalize();
            mghFile = null;
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

        return true;
    }

    /**
     * Writes a Minc file to store the image.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeMinc(final ModelImage image, FileWriteOptions options) {
        FileMinc mincFile;
        FileInfoBase fileInfo;

        if (image.getNDims() != 3) {
            MipavUtil.displayError("FileIO: MINC writer only writes 3D images.");

            return false;
        }

        try { // Construct a new file object

            if (options.isSaveAs() && !options.isSet()) {
                fileInfo = image.getFileInfo(0);
                fileInfo.setExtents(image.getExtents());

                final JDialogSaveMinc dialog = new JDialogSaveMinc(UI.getMainFrame(), fileInfo, options);

                dialog.setVisible(true);

                if (dialog.isCancelled()) {
                    return false;
                }

                options = dialog.getOptions();
            }

            mincFile = new FileMinc(options.getFileName(), options.getFileDirectory());
            createProgressBar(mincFile, options.getFileName(), FileIO.FILE_READ);
            mincFile.writeImage(image, options);
            mincFile.finalize();
            mincFile = null;

            return true;

        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug(error.getMessage() + "\n", Preferences.DEBUG_FILEIO);
            }

            error.printStackTrace();

            return false;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }
    }

    /**
     * Writes out a MINC 2.0 HDF5 formatted file
     * 
     * @param image
     * @param options
     * @return
     */
    private boolean writeMincHDF(final ModelImage image, FileWriteOptions options) {
        FileMincHDF mincFile;

        if (image.getNDims() != 3 && image.getNDims() != 4) {
            MipavUtil.displayError("FileIO: MINC writer only writes 3D and 4D images.");

            return false;
        }

        try { // Construct a new file object

            mincFile = new FileMincHDF(options.getFileName(), options.getFileDirectory());
            createProgressBar(mincFile, options.getFileName(), FileIO.FILE_READ);

            // if ( ! (image.getFileInfo()[0] instanceof FileInfoMincHDF)) {
            final JDialogSaveMinc dialog = new JDialogSaveMinc(UI.getMainFrame(), image.getFileInfo()[0], options);
            dialog.setVisible(true);

            if (dialog.isCancelled()) {
                return false;
            }

            options = dialog.getOptions();
            // }

            // if (image.getFileInfo()[0] instanceof FileInfoDicom) {

            // if ( !dataConversion( ((FileInfoDicom) image.getFileInfo()[0]), mincFile.getFileInfo())) {
            // do nothing
            // }
            // }

            mincFile.writeImage(image, options);
            mincFile.finalize();
            mincFile = null;

            return true;

        } catch (final Exception error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug(error.getMessage() + "\n", Preferences.DEBUG_FILEIO);
            }

            error.printStackTrace();

            return false;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }
    }

    /**
     * Writes a MRC file to store the image.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeMRC(final ModelImage image, final FileWriteOptions options) {
        FileMRC mrcFile;

        try { // Construct a new file object
            mrcFile = new FileMRC(options.getFileName(), options.getFileDirectory());
            createProgressBar(mrcFile, options.getFileName(), FileIO.FILE_WRITE);
            mrcFile.writeImage(image, options);
            mrcFile.finalize();
            mrcFile = null;
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

        return true;
    }

    /**
     * Writes a MATLAB file to store the image.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeMATLAB(final ModelImage image, final FileWriteOptions options) {
        FileMATLAB MATLABFile;

        try { // Construct a new file object
            MATLABFile = new FileMATLAB(options.getFileName(), options.getFileDirectory());
            createProgressBar(MATLABFile, options.getFileName(), FileIO.FILE_WRITE);
            MATLABFile.writeImage(image, options);
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

        MATLABFile.finalize();
        MATLABFile = null;
        return true;
    }

    /**
     * Writes a NIFTI file to store the image.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeNIFTI(final ModelImage image, final FileWriteOptions options) {
        FileNIFTI NIFTIFile;

        try { // Construct a new file object
            NIFTIFile = new FileNIFTI(options.getFileName(), options.getFileDirectory());
            createProgressBar(NIFTIFile, options.getFileName(), FileIO.FILE_WRITE);
            NIFTIFile.writeImage(image, options);
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

        NIFTIFile.finalize();
        NIFTIFile = null;
        return true;
    }

    /**
     * Writes a raw file to store the image.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeRaw(final ModelImage image, final FileWriteOptions options) {
        FileRaw rawFile;
        FileInfoImageXML fileInfo;

        try { // Construct new file info and file objects
            fileInfo = new FileInfoImageXML(options.getFileName(), options.getFileDirectory(), FileUtility.RAW);
            rawFile = new FileRaw(options.getFileName(), options.getFileDirectory(), fileInfo, FileBase.READ_WRITE);
            createProgressBar(rawFile, options.getFileName(), FileIO.FILE_WRITE);
            rawFile.writeImage(image, options);
            rawFile.finalize();
            rawFile = null;
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

        return true;
    }

    /**
     * Writes a SPM file to store the image.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeSPM(final ModelImage image, final FileWriteOptions options) {
        FileSPM spmFile;

        try { // Construct a new file object
            spmFile = new FileSPM(options.getFileName(), options.getFileDirectory());
            createProgressBar(spmFile, options.getFileName(), FileIO.FILE_WRITE);
            spmFile.writeImage(image, options);
            spmFile.finalize();
            spmFile = null;
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

        return true;
    }

    /**
     * Writes a TIFF file to store the image.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeTiff(final ModelImage image, final FileWriteOptions options) {
        FileTiff imageFile;
        int[] extents;

        try { // Construct a new file object
            imageFile = new FileTiff(options.getFileName(), options.getFileDirectory());
            createProgressBar(imageFile, options.getFileName(), FileIO.FILE_WRITE);

            if (LUT == null) {
                extents = new int[2];
                extents[0] = 4;
                extents[1] = 256;
                (imageFile).writeImage(image, new ModelLUT(1, 256, extents), options);
            } else {
                (imageFile).writeImage(image, LUT, options);
            }
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

        imageFile.finalize();
        imageFile = null;
        return true;
    }

    /**
     * Writes a STK file to store the image.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeSTK(final ModelImage image, final FileWriteOptions options) {
        FileSTK imageFile;
        int[] extents;

        try { // Construct a new file object
            imageFile = new FileSTK(options.getFileName(), options.getFileDirectory());
            createProgressBar(imageFile, options.getFileName(), FileIO.FILE_WRITE);

            if (LUT == null) {
                extents = new int[2];
                extents[0] = 4;
                extents[1] = 256;
                (imageFile).writeImage(image, new ModelLUT(1, 256, extents), options);
            } else {
                (imageFile).writeImage(image, LUT, options);
            }
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

        imageFile.finalize();
        imageFile = null;
        return true;
    }

    /**
     * Writes the image in our MIPAV XML format.
     * 
     * @param image The image to be saved to the file.
     * @param options Write options that control aspects of writing the image.
     * 
     * @return True if the file was successfully saved to a file.
     */
    private boolean writeXML(final ModelImage image, final FileWriteOptions options, final boolean bDisplayProgress) {
        FileImageXML xmlFile;

        try {
            xmlFile = new FileImageXML(options.getFileName(), options.getFileDirectory());
            if (bDisplayProgress) {
                createProgressBar(xmlFile, options.getFileName(), FileIO.FILE_WRITE);
            }

            /**
             * Set the LUT (for grayscale) and ModelRGB (for color) doesn't matter if either is null
             */
            xmlFile.setModelLUT(LUT);

            if (image.getFileInfo()[0] instanceof FileInfoDicom) {

                if ( !dataConversion( (image.getFileInfo()[0]), xmlFile.getFileInfo())) {
                    return false;
                }

                xmlFile.setAdditionalSets(xmlFile.getFileInfo().getPSetHashtable().elements());
            } else if (image.getFileInfo()[0] instanceof FileInfoLSM) {
                LSMDataConversion( ((FileInfoLSM) image.getFileInfo()[0]), xmlFile.getFileInfo());
                xmlFile.setAdditionalSets(xmlFile.getFileInfo().getPSetHashtable().elements());
            } else if (image.getFileInfo()[0] instanceof FileInfoMincHDF) {
                if ( !dataConversion( (image.getFileInfo()[0]), xmlFile.getFileInfo())) {
                    return false;
                }

                xmlFile.setAdditionalSets(xmlFile.getFileInfo().getPSetHashtable().elements());
            }

            xmlFile.writeImage(image, options);
            if (options.isMultiFile()) {
                dataFileName = xmlFile.getDataFileName().clone();
            }
            xmlFile.finalize();
            xmlFile = null;
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

        return true;
    }

    /**
     * writes Vista format files
     * 
     * @param image
     * @param options
     * @return
     */
    private boolean writeVista(final ModelImage image, final FileWriteOptions options) {
        try {
            FileVista fv = new FileVista(options.getFileName(), options.getFileDirectory());

            if (isQuiet()) {
                createProgressBar(fv, options.getFileName(), FileIO.FILE_WRITE);
                fv.writeImage(image, options, null);
                fv.finalize();
                fv = null;
            } else {
                final JDialogSaveVistaParams dialog = new JDialogSaveVistaParams(UI.getMainFrame(), image);

                if (dialog.isCancelled()) {
                    return false;
                }

                final ArrayList<JTextField> vistaParamFields = dialog.getVistaParamTextfields();
                dialog.dispose();

                createProgressBar(fv, options.getFileName(), FileIO.FILE_WRITE);
                fv.writeImage(image, options, vistaParamFields);
                fv.finalize();
                fv = null;
            }

        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

        return true;
    }

    /**
     * Writes a PAR/REC file to store the image.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */

    private boolean writePARREC(final ModelImage image, final FileWriteOptions options) {

        try { // Construct new file info and file objects

            final FileInfoBase fileBase = image.getFileInfo()[0];
            if ( ! (fileBase instanceof FileInfoPARREC)) {
                MipavUtil.displayError("MIPAV only supports Par/Rec to Par/Rec");
                return false;
            }
            if (fileBase.getDataType() != ModelStorageBase.FLOAT && fileBase.getDataType() != ModelStorageBase.USHORT
                    && fileBase.getDataType() != ModelStorageBase.UBYTE) {
                throw new IOException("Format PAR/REC does not support this data type.");
            }

            if (FilePARREC.isImageFile(options.getFileName())) {
                if (fileBase.getDataType() == ModelStorageBase.FLOAT) {
                    if (FileUtility.getExtension(options.getFileName()).equalsIgnoreCase(".rec")) {
                        options.setFileName(options.getFileName().substring(0, options.getFileName().length() - 4)
                                + ".frec");
                    }
                } else {
                    if (FileUtility.getExtension(options.getFileName()).equalsIgnoreCase(".frec")) {
                        options.setFileName(options.getFileName().substring(0, options.getFileName().length() - 5)
                                + ".rec");
                    }
                }
            }

            FilePARREC pr = new FilePARREC(options.getFileName(), options.getFileDirectory(), fileBase);

            createProgressBar(pr, options.getFileName(), FileIO.FILE_WRITE);
            pr.writeImage(image, options);
            pr.finalize();
            pr = null;
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

        return true;
    }

    /**
     * Writes a NRRD file to store the image.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeNRRD(final ModelImage image, final FileWriteOptions options) {
        try {
            FileNRRD fileNRRD = new FileNRRD(options.getFileName(), options.getFileDirectory());
            createProgressBar(fileNRRD, options.getFileName(), FileIO.FILE_WRITE);
            fileNRRD.writeImage(image, options);
            fileNRRD.finalize();
            fileNRRD = null;
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

        return true;
    }

    /**
     * Reads a JPEG2000 file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param one Indicates that only the named file should be read, as opposed to reading the matching files in the
     *            directory, as defined by the filetype. <code>true</code> if only want to read one image from 3D
     *            dataset.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readJpeg2000(final String fileName, final String fileDir, final boolean one) {
        ModelImage image = null;
        FileJP2 imageFile;
        // System.out.println("Hello, world!");
        try {
            progressBar = new ViewJProgressBar(fileName, FileIO.FILE_READ + fileName + " ...", 0, 100, true, null,
                    null, !quiet);
            imageFile = new FileJP2(fileName, fileDir, progressBar);
            image = imageFile.readImage(false, one);
            LUT = imageFile.getModelLUT();

        } catch (final IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        return image;
    }

    /**
     * Writes a JPEG2000 file to store the image.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeJpeg2000(final ModelImage image, final FileWriteOptions options) {
        FileJP2 imageFile;

        try { // Construct a new file object
            progressBar = new ViewJProgressBar(options.getFileName(), FileIO.FILE_WRITE + options.getFileName()
                    + " ...", 0, 100, true, null, null, !quiet);
            imageFile = new FileJP2(options.getFileName(), options.getFileDirectory(), progressBar);

            (imageFile).writeImage(image);
        } catch (final IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        }
        progressBar.dispose();
        progressBar = null;
        return true;
    }

    public void setDisplayRangeOfSlicesDialog(final boolean displayRangeOfSlicesDialog) {
        this.displayRangeOfSlicesDialog = displayRangeOfSlicesDialog;
    }

    // ~ Inner Classes
    // --------------------------------------------------------------------------------------------------

    /**
     * Orientation information held by orientation.
     */
    public class OrientStatus {

        /** DOCUMENT ME! */
        int index;

        /** DOCUMENT ME! */
        float location;

        /**
         * Creates an Orientation Status with a given index and location.
         * 
         * @param ind DOCUMENT ME!
         * @param loc DOCUMENT ME!
         */
        public OrientStatus(final int ind, final float loc) {
            index = ind;
            location = loc;
        }

        /**
         * Determines if the given orientation is the same as the current one.
         * 
         * @param o DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public boolean equals(final Object o) {

            if (o instanceof OrientStatus) {

                if ( ((OrientStatus) o).getLocation() == location) {
                    return true;
                }
            } else {
                return false;
            }

            return false;
        }

        /**
         * Provides the index.
         * 
         * @return DOCUMENT ME!
         */
        public int getIndex() {
            return index;
        }

        /**
         * Provides the value of the location.
         * 
         * @return DOCUMENT ME!
         */
        public float getLocation() {
            return location;
        }
    }

}
