package gov.nih.mipav.model.file;


import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.dicomcomm.DICOM_Constants;
import gov.nih.mipav.model.file.xcede.XCEDEElement;
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
import java.util.*;
import java.util.zip.GZIPInputStream;

import javax.imageio.ImageIO;
import javax.swing.JOptionPane;

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
    private ViewUserInterface UI;

    /**
     * Dialog to prompt the user to determine the correct file type. If the type of file to read is unknown (ie., the
     * suffix doesn't match one of the known types, build and display the unknown file dialog so the user can try to
     * identify the image type so the correct reader can be used
     */
    private JDialogUnknownIO unknownIODialog;

    /** flag telling IO this is a paint brush bitmap */
    private boolean isPaintBrush = false;

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

        unknownIODialog = new JDialogUnknownIO(UI.getMainFrame(), "Choose File Type");
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
    public FileIO(ModelLUT _LUT) {
        UI = ViewUserInterface.getReference();
        LUT = _LUT;

        unknownIODialog = new JDialogUnknownIO(UI.getMainFrame(), "Choose File Type");
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
    public static final int chkMultiFile(int fileType, boolean multiFile) {
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
    public static final ModelImage convertToARGB(ModelImage modelImage) {
        float min = (float) modelImage.getMin();
        float max = (float) modelImage.getMax();

        float[] oneSliceBuffer = new float[modelImage.getExtents()[0] * modelImage.getExtents()[1] * 4];

        ModelImage modelImageResultARGB = new ModelImage(ModelStorageBase.ARGB, modelImage.getExtents(), modelImage
                .getImageName());

        try {

            for (int i = 0; i < modelImage.getExtents()[2]; i++) // loop through images
            {
                modelImage.exportData(i * oneSliceBuffer.length, oneSliceBuffer.length, oneSliceBuffer); // export a
                // 2d buffer
                // from
                // modelImageResult

                oneSliceBuffer = resample255(oneSliceBuffer, min, max);

                modelImageResultARGB.importData(i * oneSliceBuffer.length, oneSliceBuffer, false); // import into
                // modelImageResultUB

                FileIO.copyResolutions(modelImage, modelImageResultARGB, i);
            }

            modelImageResultARGB.calcMinMax();
        } catch (Exception e) {
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
    public static final ModelImage convertToUBYTE(ModelImage modelImageResult) {
        float min = (float) modelImageResult.getMin();
        float max = (float) modelImageResult.getMax();
        float[] oneSliceBuffer = new float[modelImageResult.getExtents()[0] * modelImageResult.getExtents()[1]];

        ModelImage modelImageResultUB = new ModelImage(ModelStorageBase.UBYTE, modelImageResult.getExtents(),
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

                    oneSliceBuffer = resample255(oneSliceBuffer, min, max);

                    modelImageResultUB.importData(i * oneSliceBuffer.length, oneSliceBuffer, false); // import into
                    // modelImageResultUB

                    FileIO.copyResolutions(modelImageResult, modelImageResultUB, i);
                }
            } else {
                // 2d image
                modelImageResult.exportData(0, oneSliceBuffer.length, oneSliceBuffer);
                oneSliceBuffer = resample255(oneSliceBuffer, min, max);
                modelImageResultUB.importData(0, oneSliceBuffer, false);
                FileIO.copyResolutions(modelImageResult, modelImageResultUB, 0);
            }

            modelImageResultUB.calcMinMax();
        } catch (Exception e) {
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
    public static final ModelImage subsample(ModelImage modelImage, Dimension subsampleDimension) {
        int[] subsampledExtents = new int[] {subsampleDimension.getSize().width, subsampleDimension.getSize().height};
        // SubSample dialog now allows users to pad image so that extents are divisible by the subsampling scalar.
        int[] padExtents = modelImage.getExtents();
        ModelImage modelImageResult = new ModelImage(modelImage.getType(), new int[] {
                subsampleDimension.getSize().width, subsampleDimension.getSize().height}, modelImage.getImageName()
                + "_subsampled");

        AlgorithmSubsample algorithmSubsample = new AlgorithmSubsample(modelImage, modelImageResult, subsampledExtents,
                padExtents, new float[] {1.0f, 1.0f, 1.0f}, false, false, null, false);
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
            fileType = unknownIODialog.getImageType();
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
    public ModelImage readDicom(String selectedFileName, String[] fileList, boolean performSort) {
        ModelImage image = null;
        FileDicom imageFile;
        FileInfoDicom refFileInfo;
        FileInfoDicom[] savedFileInfos;

        float[] bufferFloat;
        short[] bufferShort;
        int[] extents;
        int length = 0;
        int nImages = 0;
        int nListImages;
        float[] tPt = new float[3];
        TransMatrix matrix = null;
        String studyID = new String();
        String seriesNo = new String();
        String acqNo = new String();
        String seriesNoRef = new String();
        String studyIDMaster;
        String seriesNoMaster;
        String acqNoMaster;

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
            imageFile.readHeader(true); // can we read the header?
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            }

            error.printStackTrace();

            return null;
        } catch (IOException error) {

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

            // TODO: should both of these always be allocated?
            bufferFloat = new float[length];
            bufferShort = new short[length];
        } catch (OutOfMemoryError error) {
            bufferFloat = null;
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
                studyIDMaster.trim();
            } else {
                studyIDMaster = "";
            }

            if (refFileInfo.getTagTable().getValue("0020,0012") != null) {
                acqNoMaster = (String) (refFileInfo.getTagTable().getValue("0020,0012"));
                acqNoMaster.trim();
            } else {
                acqNoMaster = "";
            }

            if (refFileInfo.getTagTable().getValue("0020,0011") != null) {
                seriesNoMaster = (String) (refFileInfo.getTagTable().getValue("0020,0011"));
                seriesNoRef = (String) (refFileInfo.getTagTable().getValue("0020,0011"));
                seriesNoMaster.trim();

                if (seriesNoRef.length() > 5) {
                    seriesNoRef = seriesNoMaster.substring(0, 5);
                }
            } else {
                seriesNoMaster = "";
                seriesNoRef = "";
            }

            createProgressBar(null, FileUtility.trimNumbersAndSpecial(selectedFileName)
                    + FileUtility.getExtension(selectedFileName), FILE_READ);

        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

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

        int[] indicies = null;
        int[] orient = new int[3]; // for FileInfoBase values. eg:FileInfoBase.ORI_S2I_TYPE;
        int pBarVal = 0;

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
            indicies = new int[nListImages];

            int[] zOri = new int[nListImages]; // sorted image orientation values (ie., image z-axis)
            int[] rint = new int[nListImages]; // sorted image instance values.
            float[] zOrients = new float[nListImages]; // image orientation values as read in.
            float[] instanceNums = new float[nListImages]; // image instance numbers as read in.

            // progressBar.setTitle("Reading headers");

            nImages = 0;

            for (int i = 0; i < nListImages; i++) {

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

                    // If study and series number match - Continue;
                    if (fileInfoTemp.getTagTable().getValue("0020,0010") != null) {
                        studyID = (String) (fileInfoTemp.getTagTable().getValue("0020,0010"));
                        studyID.trim();
                    }

                    if (fileInfoTemp.getTagTable().getValue("0020,0011") != null) {
                        seriesNo = (String) (fileInfoTemp.getTagTable().getValue("0020,0011"));
                        seriesNo.trim();
                    }

                    if (fileInfoTemp.getTagTable().getValue("0020,0012") != null) {
                        acqNo = (String) (fileInfoTemp.getTagTable().getValue("0020,0012"));
                        acqNo.trim();
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
                            instanceNums[nImages] = (float) savedFileInfos[nImages].instanceNumber;
                            zOri[nImages] = nImages;
                            rint[nImages] = nImages;
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
                        instanceNums[nImages] = (float) savedFileInfos[nImages].instanceNumber;
                        zOri[nImages] = nImages;
                        rint[nImages] = nImages;
                        nImages++;
                    }
                } catch (IOException error) {

                    if ( !quiet) {
                        MipavUtil.displayError("FileIO: " + error);
                        Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug(" FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
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

            // need to let the reference tag table know about the tag tables which refer to it
            FileDicomTagTable[] childrenTagTables = new FileDicomTagTable[savedFileInfos.length - 1];

            for (int i = 0, j = 0; i < savedFileInfos.length; i++) {

                if (savedFileInfos[i] != refFileInfo) {
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
             * test-datasets), we attempt to re-order the ordered Z-locations based on instance number to pull out the
             * time-data (ie., of two images with the same Z-axis, a lower instance number was taken at an earlier time;
             * all such earlier-time images are grouped). If neither solution seemed to work, then we order all the
             * images strictly on the instance number. There might be some problems if we had to do this. If the list
             * sent to this method was found using the getFileList() method, all the names are in lexicographical order.
             * This is the default ordering if neither sorting method works, or if performSort is false.
             */
            boolean valid = false; // !valid, !performSort, !fourthDimensional

            // will force sorting to go by input order.
            boolean fourthDimensional = false; // 4th dimensional

            if (performSort) {

                // sort so that instance numbers are in ascending order.
                // rint is the index to associate input file-list with the
                // instance number
                if ( !sort(instanceNums, rint, nImages)) {
                    Preferences.debug("FileIO: instance numbers sort failed\n", Preferences.DEBUG_FILEIO);
                    System.err.println("FileIO: instance numbers sort failed on " + fileList[0]);
                }

                valid = true; // original ordering in case nImages == 1;

                // sort so that zOrients is now in ascending order.
                // zOri[i] represents where in the image buffer image
                // number i should be stored; so that if the images were
                // read in 1, 10, 11... (which happens often),
                // zOri[1] = 1 but zOri[2] = 2, rather than 10.
                if (nImages > 1) {
                    valid = sort(zOrients, zOri, nImages);
                }

                // If valid is false then one or more of the images has the
                // same position. Most likely it is a 4D dataset.
                // let's deal with that possibility:
                if ( (nImages > 1) && !valid) {

                    // Follow-on ordering:
                    // pre-order the orientation numbers to match the instance
                    // numbers. This is done to accomodate 4D dicom sets.
                    // To describe the algo: I and L value lists which are
                    // independant of each other, and M, which is a copy of L.
                    // a & b are index lists for I and L, respectivly.
                    // L[b[z]] = M[a[z]]
                    // we copy the values of M, using order a, into L, using
                    // order b, thereby making L mimic I.
                    // we do this here with zOrients/zOri to make zOrient match
                    // the ordering of the Instance numbers.
                    float[] lima = new float[zOrients.length]; // temp

                    System.arraycopy(zOrients, 0, lima, 0, zOrients.length);

                    for (int z = 0; z < nImages; z++) {
                        zOrients[zOri[z]] = lima[rint[z]]; // copy z-location
                    }

                    zOri = rint; // copy the indexing

                    sort(zOrients, zOri, nImages); // now sort by orientation

                    // Rely on image instance number and position information.
                    // Build a list for all images at a particular location,
                    // although at different times, as judged by image instance.
                    // Create a list for all possible times in the imageset:
                    Vector<Vector<OrientStatus>> timezonesList = new Vector<Vector<OrientStatus>>();

                    // Hold the original list of orients and indices:
                    Vector<OrientStatus> orientsList = new Vector<OrientStatus>(nImages); // original index list

                    for (int k = 0; k < nImages; k++) { // load original list vector

                        OrientStatus oriReference = new OrientStatus(zOri[k], zOrients[k]);

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

                        for (Enumeration<OrientStatus> e = orientsClone.elements(); e.hasMoreElements();) {
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

                        for (Enumeration<Vector<OrientStatus>> e = timezonesList.elements(); e.hasMoreElements();) {
                            Preferences.debug("Timezone\n", Preferences.DEBUG_FILEIO);

                            for (Enumeration<OrientStatus> f = e.nextElement().elements(); f.hasMoreElements();) {
                                OrientStatus ref = f.nextElement();
                                int k = ref.getIndex();

                                // concatenate the sublists into one ordering list.
                                zOri[k] = t;
                                zOrients[k] = ref.getLocation();
                                Preferences.debug("reordering: (" + k + "): " + zOri[k] + "..." + zOrients[k] + "\n",
                                        Preferences.DEBUG_FILEIO);
                                t++;
                            }
                        }

                        fourthDimensional = true;
                        Preferences.debug("4D, translation passes!  " + t + " slices!\n", Preferences.DEBUG_FILEIO);
                    } catch (ArrayIndexOutOfBoundsException enumTooFar) {
                        fourthDimensional = false;
                        Preferences.debug("NOT 4D, and DICOM translation fail!\n", Preferences.DEBUG_FILEIO);
                    }

                    timezonesList.clear();
                    timezonesList = null;
                    System.gc();
                } else {
                    Preferences.debug("Not a 4D Dataset\n", Preferences.DEBUG_FILEIO);
                }
                // System.out.println (" Dicom matrix = \n" + matrix + "\n");
                // System.out.println (" valid = " + valid);

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
                xCos = (float) Math.abs(matrix.get(2, 0));
                yCos = (float) Math.abs(matrix.get(2, 1));
                zCos = (float) Math.abs(matrix.get(2, 2));

                for (int j = 0; j < orient.length; j++) {
                    int index = absBiggest(matrix.get(j, 0), matrix.get(j, 1), matrix.get(j, 2));

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

                if (valid) {
                    indicies = zOri;
                } else if ( !valid && fourthDimensional) {
                    indicies = zOri;
                    Preferences.debug("Reading image as 4th Dimensional.\n", Preferences.DEBUG_FILEIO);
                } else {
                    indicies = rint;
                }
            } else if ( (yCos > xCos) && (yCos > zCos)) {
                orientation = FileInfoBase.CORONAL;

                if (valid == true) {
                    indicies = zOri;
                } else if ( !valid && fourthDimensional) {
                    indicies = zOri;
                    Preferences.debug("Reading image as a 4th Dimensional " + "order.\n", Preferences.DEBUG_FILEIO);
                } else {
                    indicies = rint;
                }
            } else if ( (zCos > xCos) && (zCos > yCos)) {
                orientation = FileInfoBase.AXIAL;

                if (valid == true) {
                    indicies = zOri;
                } else if ( !valid && fourthDimensional) {
                    indicies = zOri;
                    Preferences.debug("Reading image as a 4th Dimensional " + "order.\n", Preferences.DEBUG_FILEIO);
                } else {
                    indicies = rint;
                }
            } // matrix was null, set orients based on instance number

            // problems if we reach this point!
            else if ( (instanceNums.length > 1) && (instanceNums[0] != instanceNums[1])) {
                orientation = FileInfoBase.AXIAL;
                indicies = rint;
                orient[0] = FileInfoBase.ORI_R2L_TYPE;
                orient[1] = FileInfoBase.ORI_A2P_TYPE;
                orient[2] = FileInfoBase.ORI_I2S_TYPE;
            } else { // xLocation, yLocation, and zLocation were probably undefined and instance numbers equal.
                orientation = FileInfoBase.AXIAL;
                indicies = zOri;
                orient[0] = FileInfoBase.ORI_R2L_TYPE;
                orient[1] = FileInfoBase.ORI_A2P_TYPE;
                orient[2] = FileInfoBase.ORI_I2S_TYPE;
            }
        }

        if (nImages > 1) {
            extents = new int[3];
            extents[2] = nImages;
        } else {
            extents = new int[2];
        }

        extents[0] = refFileInfo.getExtents()[0];
        extents[1] = refFileInfo.getExtents()[1];
        refFileInfo.setExtents(extents);

        image = new ModelImage(refFileInfo.displayType, extents, studyIDMaster.trim() + "_" + seriesNoRef.trim());

        if (refFileInfo.isMultiFrame() == true) {
            image.setFileInfo(refFileInfo, 0);
        }

        // its probably a PET image an therefore reallocate data to store float image.
        if (refFileInfo.displayType != refFileInfo.getDataType()) {

            // TODO: the image was just created with refFileInfo.displayType... does this realloc have any effect
            image.setType(refFileInfo.displayType);
            image.reallocate(refFileInfo.displayType);
        }

        String filename;
        int start;
        int location;
        boolean multiframe = refFileInfo.isMultiFrame();

        // loop through files, place them in image array
        pBarVal = 0;

        for (int i = 0; i < nImages; i++) {

            if (multiframe) {
                filename = fileList[0];
                start = i;
                location = i;
            } else {
                filename = fileList[i];
                start = 0;
                location = indicies[i];

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

                // Reuse header that was read in above !!!!
                FileInfoDicom curFileInfo;

                if ( !multiframe) {
                    curFileInfo = savedFileInfos[i];
                } else {
                    curFileInfo = refFileInfo;
                }

                imageFile.setFileInfo(curFileInfo);

                // Read the image
                if (image.getType() == ModelStorageBase.FLOAT) {
                    imageFile.readImage(bufferFloat, curFileInfo.getDataType(), start);
                } else {
                    imageFile.readImage(bufferShort, curFileInfo.getDataType(), start);
                }

                curFileInfo.setExtents(extents);
                curFileInfo.setAxisOrientation(orient);
                curFileInfo.setImageOrientation(orientation);

                float[] origin = new float[3];
                float[] newOriginPt = new float[3];

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

                curFileInfo.setOrigin(newOriginPt);
                image.setFileInfo(curFileInfo, location);

                if (image.getType() == ModelStorageBase.FLOAT) {
                    image.importData(location * length, bufferFloat, false);
                } else {
                    image.importData(location * length, bufferShort, false);
                }
            } catch (IOException error) {

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
            } catch (OutOfMemoryError error) {

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
            TransMatrix invMatrix = ((TransMatrix) (matrix.clone()));

            try {
                invMatrix.invert();
                invMatrix.setTransformID(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL);

                image.getMatrixHolder().addMatrix(invMatrix);

                // image.setMatrix(invMatrix);

                // for (int m = 0; m < nImages; m++) {
                // image.getFileInfo(m).setTransformID(FileInfoBase.TRANSFORM_SCANNER_ANATOMICAL);
                // }
            } catch (RuntimeException rte) {
                invMatrix.identity();
                // MipavUtil.displayError("Error = " + rte);
            }
        }

        if (nListImages > 1) {

            for (int m = 0; m < nImages; m++) {
                image.getFileInfo(m).setMultiFile(true);
            }
        }

        FileDicomTagTable firstSliceTagTable = ((FileInfoDicom) image.getFileInfo(0)).getTagTable();

        if (nImages > 1) {

            float sliceThickness = -1;
            float sliceSpacing = -1;

            // First check slice thickness tag:
            if ( (firstSliceTagTable.get("0018,0050") != null) || (firstSliceTagTable.get("0018,0088") != null)) {

                if ((String) firstSliceTagTable.getValue("0018,0050") != null) {
                    try {
                        sliceThickness = Float.parseFloat((String) firstSliceTagTable.getValue("0018,0050"));
                    } catch (NumberFormatException nfe) {
                        Preferences.debug("0018,0050:\tInvalid float value found in slice thickness tag.",
                                Preferences.DEBUG_FILEIO);
                    }
                }

                // 0018,0088 = Spacing Between Slices
                // 0018,0050 = Slice Thickness
                if ((String) firstSliceTagTable.getValue("0018,0088") != null) {
                    try {
                        sliceSpacing = Float.parseFloat((String) firstSliceTagTable.getValue("0018,0088"));
                    } catch (NumberFormatException nfe) {
                        Preferences.debug("0018,0088:\tInvalid float value found in slice spacing tag.",
                                Preferences.DEBUG_FILEIO);
                    }
                }

                // System.err.println("Slice Spacing: " + sliceSpacing);
                if (sliceSpacing != -1) {
                    sliceThickness = sliceSpacing;
                    // System.err.println("Slice Thickness: " + sliceThickness);
                }

                if (sliceThickness > 0) {

                    for (int m = 0; m < nImages; m++) {
                        image.getFileInfo(m).setSliceThickness(sliceThickness);
                    }
                }
            }

            // finally, if slice spacing and slice location failed to produce a z-res,
            // check for image position
            if (firstSliceTagTable.get("0020,0032") != null) {

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
                }
            }

            float sliceDifference = -1;

            // if slice thickness tag wasn't there or was 0, check slice location (and take the difference)
            if ( (firstSliceTagTable.get("0020,1041") != null) && (sliceThickness == -1)) {

                if ((String) firstSliceTagTable.getValue("0020,1041") != null) {
                    sliceDifference = Float.parseFloat((String) ((FileInfoDicom) image.getFileInfo(1)).getTagTable()
                            .getValue("0020,1041"))
                            - Float.parseFloat((String) firstSliceTagTable.getValue("0020,1041"));

                    // System.err.println("Slice difference: " + sliceDifference);

                    // TODO: is this check ever true?
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
                }
            }

            // see if we found z-res somewhere
            if ( (sliceThickness == -1) && (sliceDifference == -1)) {
                System.err.println("error calculating z-resolution in FileIO.readDicom()");
            }
        }

        // That the image contains this tag, means that the image contains it's own
        // LUT, and that we should use it.
        if (firstSliceTagTable.get("0028,1201") != null) {
            LUT = imageFile.getLUT();
        }

        if (progressBar != null) {
            progressBar.dispose();
        }
        imageFile.finalize();
        imageFile = null;
        matrix = null;

        return image;
    }

    /**
     * Reads generic file from an absolute filename.
     * 
     * @param absoluteFilename String - the absolute filename, including the path
     * 
     * @return ModelImage
     */
    public ModelImage readImage(String absoluteFilename) {

        if (absoluteFilename == null) {
            return null;
        }

        int lastIndex = absoluteFilename.lastIndexOf(File.separatorChar);

        if (lastIndex == -1) {

            // failed
            return null;
        }

        String path = absoluteFilename.substring(0, lastIndex);
        String filename = absoluteFilename.substring(lastIndex);

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
    public ModelImage readImage(String fileName, String fileDir) {
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
    public ModelImage readImage(String fileName, String fileDir, boolean multiFile, FileInfoBase fileInfo) {
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
    public ModelImage readImage(String fileName, String fileDir, boolean multiFile, FileInfoBase fileInfo, boolean loadB) {
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
    public ModelImage readImage(String fileName, String fileDir, boolean multiFile, FileInfoBase fileInfo,
            int secondAddress) {
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
    public ModelImage readImage(String fileName, String fileDir, boolean multiFile, FileInfoBase fileInfo,
            int secondAddress, boolean loadB, boolean one) {
        int index;
        boolean gunzip;
        File file;
        FileInputStream fis;
        GZIPInputStream gzin;
        FileOutputStream out;
        int bytesRead;
        ModelImage image = null;
        int fileType = FileUtility.UNDEFINED;
        int userDefinedFileType = 0;
        String userDefinedSuffix = null;
        if ( (fileName == null) || (fileDir == null)) {
            return null;
        }

        this.fileDir = fileDir;
        fileName.trim();

        index = fileName.lastIndexOf(".");

        if (fileName.substring(index + 1).equalsIgnoreCase("gz")) {
            gunzip = true;
        } else {
            gunzip = false;
        }

        file = new File(fileDir + fileName);

        if (gunzip) {
            int totalBytesRead = 0;

            try {
                fis = new FileInputStream(file);
            } catch (FileNotFoundException e) {
                MipavUtil.displayError("File not found exception on fis = new FileInputStream(file) for " + fileName);
                return null;
            }

            try {
                gzin = new GZIPInputStream(new BufferedInputStream(fis));
            } catch (IOException e) {
                MipavUtil.displayError("IOException on GZIPInputStream for " + fileName);
                return null;
            }

            fileName = fileName.substring(0, index);
            String uncompressedName = fileDir + fileName;
            try {
                out = new FileOutputStream(uncompressedName);
            } catch (IOException e) {
                MipavUtil.displayError("IOException on FileOutputStream for " + uncompressedName);
                return null;
            }
            byte[] buffer = new byte[256];

            while (true) {
                try {
                    bytesRead = gzin.read(buffer);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException on gzin.read(buffer) for " + uncompressedName);
                    return null;
                }

                if (bytesRead == -1) {
                    break;
                }

                totalBytesRead += bytesRead;
                try {
                    out.write(buffer, 0, bytesRead);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException on out.write for " + uncompressedName);
                    return null;
                }
            }

            try {
                out.close();
            } catch (IOException e) {
                MipavUtil.displayError("IOException on out.close for " + uncompressedName);
                return null;
            }
        } // if (gunzip)
        fileType = FileUtility.getFileType(fileName, fileDir, false, quiet); // set the fileType

        if (fileType == FileUtility.ERROR) {
            return null;
        }

        if (fileType == FileUtility.UNDEFINED) { // if image type not defined by extension, popup
            fileType = getFileType(); // dialog to get user to define image type
            userDefinedFileType = fileType;
            if (fileName.indexOf(".") != -1) {
                userDefinedSuffix = "." + fileName.split("\\.")[1];
            }
        }

        fileType = chkMultiFile(fileType, multiFile); // for multifile support...

        try {

            switch (fileType) {

                case FileUtility.LIFF:
                    image = readLIFF(fileName, fileDir, one);
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

                case FileUtility.MGH:
                    image = readMGH(fileName, fileDir, one);
                    break;

                case FileUtility.NIFTI:
                    image = readNIFTI(fileName, fileDir, one);
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

                case FileUtility.OSM:
                    image = readOSM(fileName, fileDir);
                    break;

                case FileUtility.BFLOAT:
                    image = readBFLOAT(fileName, fileDir, one);
                    break;

                case FileUtility.MAGNETOM_VISION:
                    image = readMagnetomVision(fileName, fileDir, one);
                    break;

                case FileUtility.GE_GENESIS:
                    image = readGEGenesis5X(fileName, fileDir);
                    break;

                case FileUtility.GE_SIGNA4X:
                    image = readGESigna4X(fileName, fileDir);
                    break;

                case FileUtility.MICRO_CAT:
                    image = readMicroCat(fileName, fileDir, one);
                    break;

                case FileUtility.XML:
                    image = readXML(fileName, fileDir, one);
                    break;

                case FileUtility.XML_MULTIFILE:
                    image = readXMLMulti(fileName, fileDir);
                    break;

                case FileUtility.PARREC:
                    image = readPARREC(fileName, fileDir, one);
                    break;

                default:
                    return null;
            }

            if (image != null) {
                if (ProvenanceRecorder.getReference().getRecorderStatus() == ProvenanceRecorder.RECORDING) {
                    int idx = fileName.lastIndexOf(".");
                    String fName;
                    if (idx == -1) {
                        fName = new String(fileName);
                    } else {
                        fName = fileName.substring(0, idx);
                    }
                    if (new File(fileDir + File.separator + fName + ".xmp").exists()) {
                        try {
                            FileDataProvenance fdp = new FileDataProvenance(fName + ".xmp", fileDir, image
                                    .getProvenanceHolder());

                            fdp.readHeader(fName + ".xmp", fileDir, Preferences.DATA_PROVENANCE_SCHEMA);

                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                    }
                }

                // tell mipav system data provenance to record this opening of an image
                if ( !isPaintBrush) {
                    ProvenanceRecorder.getReference().addLine(new ActionOpenImage(image));
                }

                if (progressBar != null) {
                    progressBar.dispose();
                }

                if ( (image.getType() == ModelStorageBase.COMPLEX) || (image.getType() == ModelStorageBase.DCOMPLEX)) {
                    image.calcMinMaxMag(true);
                } else {
                    image.calcMinMax();
                    // image.setImageDirectory(fileDir);
                }

                // if file type was a new user defined file type, then we need to save its association
                // to the preferences if its not already there.
                // we also need to save this pref as part of both the userDefinedFileTypes and
                // the userDefinedFileTypes_textField
                if (userDefinedSuffix != null) {
                    String association = userDefinedSuffix + Preferences.DEFINITION_SEPARATOR + userDefinedFileType;
                    boolean isPresent = false;

                    if (Preferences.getProperty(Preferences.PREF_USER_FILETYPE_ASSOC) != null) {

                        if ( !Preferences.getProperty(Preferences.PREF_USER_FILETYPE_ASSOC).trim().equals("")) {
                            String[] userDefinedFileTypeAssociations = Preferences.getProperty(
                                    Preferences.PREF_USER_FILETYPE_ASSOC).split(Preferences.ITEM_SEPARATOR);

                            for (int i = 0; i < userDefinedFileTypeAssociations.length; i++) {

                                if (userDefinedSuffix.equals(userDefinedFileTypeAssociations[i]
                                        .split(Preferences.DEFINITION_SEPARATOR)[0])) {
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
        } catch (Exception error) {

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
    public ModelImage readOneImage(String fileName, String fileDir) {
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
    public ModelImage readOrderedARGB(File[] fileList, int numChannels, int[] channelMap,
            boolean showOrderedProgressBar, Dimension subsampleDimension, boolean forceUBYTE) {
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
                modelImageTemp = subsample(modelImageTemp, subsampleDimension);
            }

            imageExtents = modelImageTemp.getExtents();

            sliceSize = imageExtents[0] * imageExtents[1]; // width * height

            resultImageBuffer = new float[sliceSize * 4]; // the * 4 is because there are 4 channels - ARGB

            oneSliceBuffer = new float[sliceSize];
        } catch (Exception ioe) {
            ioe.printStackTrace();

            return null;
        }

        createProgressBar(null, "files", FILE_READ);

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
            int channel = channelMap[n % numChannels];
            int currentSlice = (int) (n / numChannels);

            if (fileList[n].exists()) {

                try {
                    modelImageTemp = readOneImage(fileList[n].getName(), fileList[n].getParentFile().getAbsolutePath()
                            + File.separator);

                    if (subsampleDimension != null) {
                        modelImageTemp = subsample(modelImageTemp, subsampleDimension);
                    }

                    modelImageTemp.exportData(0, oneSliceBuffer.length, oneSliceBuffer); // export one slice at a
                    // time
                    // to result ModelImage

                    for (int i = 0; i < oneSliceBuffer.length; i++) {
                        resultImageBuffer[ (i * 4) + channel] = oneSliceBuffer[i]; // arrange interleaved pixels
                    }

                    modelImageResult.importData(currentSlice * sliceSize * 4, resultImageBuffer, false);
                    copyResolutions(modelImageTemp, modelImageResult, currentSlice);

                    modelImageTemp.disposeLocal(false);
                } catch (IOException ioe) {
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
            return convertToARGB(modelImageResult);
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
    public ModelImage readOrderedGrayscale(File[] fileList, boolean showLocalProgressBar, Dimension subsampleDimension,
            boolean forceUBYTE) {
        ModelImage modelImageTemp = null;
        ModelImage modelImageResult = null;
        float[] oneSliceBuffer;

        createProgressBar(null, "files", FILE_READ);

        try {

            // read one image so we can get extents
            modelImageTemp = readOneImage(fileList[0].getName(), fileList[0].getParentFile().getAbsolutePath()
                    + File.separator);

            if (subsampleDimension != null) // subsample the image if we have subsampling dimensions
            {
                modelImageTemp = subsample(modelImageTemp, subsampleDimension);
            }

            // create a buffer to hold exchange image data between temp and result images
            oneSliceBuffer = new float[modelImageTemp.getExtents()[0] * modelImageTemp.getExtents()[1]];

            // the first slice has already been read. instead of re-reading it in the loop, export to buffer and save
            // an iteration
            modelImageTemp.exportData(0, oneSliceBuffer.length, oneSliceBuffer);

            // the result image's dimensions (possibly subsampled dimensions)
            int[] extents = {modelImageTemp.getExtents()[0], modelImageTemp.getExtents()[1], fileList.length};

            modelImageResult = new ModelImage(modelImageTemp.getType(), extents, modelImageTemp.getImageName());
            copyResolutions(modelImageTemp, modelImageResult, 0); // save the resolutions from the file info structure

            extents = null;
            modelImageTemp.disposeLocal(false);

            // import first slice to result image from modelImageTemp
            modelImageResult.importData(0, oneSliceBuffer, false);

            progressBar.updateValue((int) ( (1.0f / (float) fileList.length) * 100), false);

            for (int i = 1; i < fileList.length; i++) {

                if (fileList[i].exists()) {

                    try {

                        // read images one slice at a time
                        modelImageTemp = readOneImage(fileList[i].getName(), fileList[i].getParentFile()
                                .getAbsolutePath()
                                + File.separator);

                        if (subsampleDimension != null) // subsample if we have subsampling dimensions
                        {
                            modelImageTemp = subsample(modelImageTemp, subsampleDimension);
                        }

                        modelImageTemp.exportData(0, oneSliceBuffer.length, oneSliceBuffer);
                        copyResolutions(modelImageTemp, modelImageResult, i);

                        modelImageResult.importData(i * oneSliceBuffer.length, oneSliceBuffer, false);
                    } catch (IOException ioe) {
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

                return convertToUBYTE(modelImageResult);
            }

            return modelImageResult;
        } catch (Exception ioe) {
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
     * Reads the XCEDE schema file.
     * 
     * @param fileName the file name of the XCEDE file.
     * @param directory the directory of the XCEDE file.
     * 
     * @return the root element of the XCEDE schema.
     */
    public XCEDEElement readXCEDE(String fileName, String directory) {
        FileXCEDEXML xcedeFile;
        xcedeFile = new FileXCEDEXML(fileName, directory);

        return xcedeFile.parse();
    }

    /**
     * Reads a thumbnail image from an XML file. if thumbnail is empty, will returned FileImageXML will be null.
     * 
     * @param name filename
     * @param directory file's directory
     * 
     * @return FileImageXML containing thumbnail or null
     */
    public FileImageXML readXMLThumbnail(String name, String directory) {
        FileImageXML xmlTemp = new FileImageXML(name, directory);
        float[][] res = null;

        try {
            TalairachTransformInfo talairach = new TalairachTransformInfo();
            res = xmlTemp.readHeader(name, directory, talairach);

            if ( (res != null) && (xmlTemp.getThumbnail() != null)) {
                return xmlTemp;
            }
        } catch (IOException ioex) {
            System.err.println("Got IOException");
        }

        return null;
    }

    /**
     * Sets the directory where the file will be saved or opened.
     * 
     * @param dir String directory
     */
    public void setFileDir(String dir) {
        this.fileDir = dir;
    }

    /**
     * Sets the LUT.
     * 
     * @param lut the lookup table.
     */
    public void setModelLUT(ModelLUT lut) {
        this.LUT = lut;
    }

    /**
     * Sets the RGB.
     * 
     * @param rgb lut the lookup table.
     */
    public void setModelRGB(ModelRGB rgb) {
        this.modelRGB = rgb;
    }

    /**
     * Sets the progress bar (either panel or frame) to be used in image opening to update status.
     * 
     * @param pBar ProgressBarInterface
     */
    public void setPBar(ProgressBarInterface pBar) {
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
    public void setQuiet(boolean q) {
        quiet = q;
    }

    /**
     * Tells IO that this is a paint brush image to be loaded
     * 
     * @param isPB if this is a paint brush
     */
    public void setIsPaintBrush(boolean isPB) {
        this.isPaintBrush = isPB;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param rawInfo DOCUMENT ME!
     */
    public void setRawImageInfo(RawImageInfo rawInfo) {
        this.rawInfo = rawInfo;
    }

    /**
     * This method sets the userDefinedFileTypes_textField preference.
     * 
     * @param udefSuffix the user defined suffix
     */

    public void setUserDefinedFileTypes_textFieldPref(String udefSuffix) {

        if (Preferences.getProperty(Preferences.PREF_USER_FILETYPES_TEXTFIELDS) != null) {

            if (Preferences.getProperty(Preferences.PREF_USER_FILETYPES_TEXTFIELDS).trim().equals("")) {
                Preferences.setProperty(Preferences.PREF_USER_FILETYPES_TEXTFIELDS, "*" + udefSuffix);
            } else {

                // first check to see if its already not there
                String[] prefTypes = Preferences.getProperty(Preferences.PREF_USER_FILETYPES_TEXTFIELDS).split(";");
                boolean isPresent = false;

                for (int i = 0; i < prefTypes.length; i++) {
                    String suff = prefTypes[i].split("\\.")[1];
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
    public void setUserDefinedFileTypesPref(String udefSuffix) {

        if (Preferences.getProperty(Preferences.PREF_USER_FILETYPES) != null) {

            if (Preferences.getProperty(Preferences.PREF_USER_FILETYPES).trim().equals("")) {
                Preferences.setProperty(Preferences.PREF_USER_FILETYPES, "*" + udefSuffix);
            } else {

                // first check to see if its already not there
                String[] prefTypes = Preferences.getProperty(Preferences.PREF_USER_FILETYPES).split(";");
                boolean isPresent = false;

                for (int i = 0; i < prefTypes.length; i++) {
                    String suff = prefTypes[i].split("\\.")[1];
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
    public void writeImage(ModelImage image, FileWriteOptions options) {
        int fileType;
        String suffix;

        // set it to quiet mode (no prompting) if the options were
        // created during a script
        if (options.isScript() == true) {
            quiet = true;
        }

        if (options.isSaveAs()) { // if we're doing a save-as op, then try to get the filetype from the name
            fileType = FileUtility.getFileType(options.getFileName(), options.getFileDirectory(), true, quiet);

            // System.err.println("FileType: " + fileType);

            options.setDefault(true); // this would already be set.... hrmm....
        } else { // otherwise, get the file-type from the file-info.
            fileType = image.getFileInfo(0).getFileFormat();
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
            int index  = (options.getFileName()).lastIndexOf('.');
            if (index > 0) {
                String firstSuffix = (options.getFileName()).substring(index);
                if (firstSuffix.toUpperCase().equals(suffix.toUpperCase())) {
                    // Prevent generating a double suffix as in test.img.img
                    append = false;
                }
                else if ((firstSuffix.toUpperCase().equals(".IMG")) && (suffix.toUpperCase().equals(".NII"))) {
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
        } else if (fileType == FileUtility.MINC) {
            fileType = new JDialogSaveMincVersionChoice(ViewUserInterface.getReference().getMainFrame()).fileType();
        }

        if ( !options.isSet()) {
            options.setFileType(fileType);
            options.setMultiFile(image.getFileInfo(0).getMultiFile());
            options.setPackBitEnabled( (fileType == FileUtility.TIFF)
                    && ( (image.getFileInfo(0).getDataType() == ModelStorageBase.BYTE) || (image.getFileInfo(0)
                            .getDataType() == ModelStorageBase.UBYTE)));
        }

        if (options.isSaveAs() && !options.isSet()) {
            if ( !callDialog(image.getExtents(), (fileType == FileUtility.TIFF), options)) {
                return;
            }
        }

        boolean success = false;

        switch (fileType) {

            case FileUtility.RAW:
                success = writeRaw(image, options);
                break;

            case FileUtility.ANALYZE:
                success = writeAnalyze(image, options);
                break;

            case FileUtility.NIFTI:
                success = writeNIFTI(image, options);
                break;

            case FileUtility.SPM:
                success = writeSPM(image, options);
                break;

            case FileUtility.TIFF:
                success = writeTiff(image, options);
                break;

            case FileUtility.MGH:
                success = writeMGH(image, options);
                break;

            case FileUtility.DICOM:
                success = writeDicom(image, options);
                break;

            case FileUtility.JIMI:
                success = writeJimi(image, options);
                break;

            case FileUtility.MINC:
                success = writeMinc(image, options);
                break;

            case FileUtility.MINC_HDF:
                success = writeMincHDF(image, options);
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

            case FileUtility.OSM:
                success = writeOSM(image, options);
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
                success = writeXML(image, options);
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

            default:
                if ( !quiet) {
                    MipavUtil
                            .displayError("File type unknown.  Try Save Image As; \notherwise, the file type is not supported.");
                }

                return;
        }

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
                } catch (Exception e) {}
            }
        }

        // if the flag is set to put the image into the quicklist, do so
        if (success && options.doPutInQuicklist()) {

            if (options.isMultiFile()) {
                String fName = options.getFileName();
                String start = Integer.toString(options.getStartNumber());
                int numDig = options.getDigitNumber();

                for (int i = 1; i < numDig; i++) {
                    start = "0" + start;
                }

                fName = fName.substring(0, fName.indexOf(".")) + start
                        + fName.substring(fName.indexOf("."), fName.length());

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

            // updates menubar for each image
            Vector<Frame> imageFrames = UI.getImageFrameVector();

            if (imageFrames.size() < 1) {
                UI.buildMenu();
                UI.setControls();
            } else {
                UI.buildMenu();

                for (int i = 0; i < imageFrames.size(); i++) {

                    if (imageFrames.elementAt(i) instanceof ViewJFrameImage) {
                        ((ViewJFrameImage) (imageFrames.elementAt(i))).updateMenubar();
                    }
                }

                UI.getActiveImageFrame().setControls();
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

    /**
     * Writes project information to a file.
     * 
     * @param projectInfo The project information to be written to the file
     * @param options Write options that control aspects of writing the project information.
     * 
     * @return True if the file was successfully saved to a file.
     */
    public boolean writeProject(FileInfoProject projectInfo, FileWriteOptions options) {
        FileProject projectFile;

        try {
            projectFile = new FileProject(options.getFileName(), options.getFileDirectory());

            // System.out.println( "writing project" );
            projectFile.writeProject(projectInfo, options.getFileName(), options.getFileDirectory());
        } catch (IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

        return true;
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
    protected boolean dataConversion(FileInfoBase sourceInfo, FileInfoBase destInfo) {

        // when the original image is a DICOM or MINCimage, we want to save this as
        // XML, so look, or ask, for a dicom dictionary list of tags to save
        // into the XML
        // load the tags to keep:

        Hashtable<FileDicomKey, FileDicomTag> tags2save = null;
        if (sourceInfo instanceof FileInfoDicom) {
            tags2save = getDicomSaveList((FileInfoDicom) sourceInfo, destInfo instanceof FileInfoImageXML);
        } else if (sourceInfo instanceof FileInfoMincHDF) {
            tags2save = ((FileInfoMincHDF) sourceInfo).getDicomTable();
        }

        if (tags2save == null) {
            return false;
        }

        if (destInfo instanceof FileInfoImageXML) {
            FileInfoImageXML dInfo = (FileInfoImageXML) destInfo;

            // now convert that DICOM tags list into an XML tags List:
            Enumeration<FileDicomKey> e = tags2save.keys();
            while (e.hasMoreElements()) {
                FileDicomKey tagKey = e.nextElement();
                FileDicomTag dicomTag = tags2save.get(tagKey);

                dInfo.createPSet(dicomTag.getName());

                Object[] tagValues = new Object[0];

                try {
                    tagValues = dicomTag.getValueList();
                } catch (NullPointerException npe) {
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

                        if (tagValues[q].toString().indexOf(0) != -1) {

                            // if there are NULL characters, remove them
                            // before saving the value
                            StringBuffer noNulls = new StringBuffer(tagValues[q].toString());

                            try {

                                while (noNulls.indexOf("\u0000") != -1) { // removing NULL
                                    noNulls.deleteCharAt(noNulls.indexOf("\u0000"));
                                }

                                dInfo.getCurrentPSet().getCurrentParameter().setValue(noNulls.toString());
                            } catch (StringIndexOutOfBoundsException nullNotThere) {
                                dInfo.getCurrentPSet().getCurrentParameter().setValue("");
                                System.err.println("(" + tagKey + ") Trying to output " + "current string bounded by "
                                        + "colons, nulls and all:");

                                try {
                                    System.err.println(":" + noNulls.toString() + ":");
                                } catch (Exception couldnt) {
                                    System.err.println("...Couldn't output noNulls string.");
                                }

                                Preferences.debug("Error converting DICOM to XML.", Preferences.DEBUG_FILEIO);
                                Preferences.debug("  Empty string written for :", Preferences.DEBUG_FILEIO);
                                Preferences.debug(" (" + tagKey + ")\n", Preferences.DEBUG_FILEIO);
                            }
                        } else {
                            dInfo.getCurrentPSet().getCurrentParameter().setValue(tagValues[q].toString());
                        }
                    } catch (NullPointerException npe) {
                        Preferences.debug("Error converting DICOM to XML.", Preferences.DEBUG_FILEIO);
                        Preferences.debug("  Empty string written for:", Preferences.DEBUG_FILEIO);
                        Preferences.debug(" (" + tagKey + ")\n", Preferences.DEBUG_FILEIO);
                        dInfo.getCurrentPSet().getCurrentParameter().setValue("");
                    }
                }
            }
        } else if (destInfo instanceof FileInfoMincHDF) {
            ((FileInfoMincHDF) destInfo).setDicomTable(tags2save);
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
    protected Hashtable<FileDicomKey, FileDicomTag> getDicomSaveList(FileInfoDicom sourceInfo, boolean isXML) {
        Hashtable<FileDicomKey, FileDicomTagInfo> tags2save;

        if (quiet) { // don't bother asking user when running a macro.
            tags2save = DicomDictionary.getSubsetDicomTagTable();

            if (tags2save == null) {
                System.out.println("tags2save is null");
            }
        } else {
            JDialogDicom2XMLSelection jdl = new JDialogDicom2XMLSelection(sourceInfo, isXML);
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

        Hashtable<FileDicomKey, FileDicomTag> fullTagsList = sourceInfo.getTagTable().getTagList();
        Hashtable<FileDicomKey, FileDicomTag> tagsWithValues = new Hashtable<FileDicomKey, FileDicomTag>(Math.min(
                tags2save.size(), fullTagsList.size()));

        // place DICOM tags (with values) into the save-tags list.
        // Remove any tags from the list that do not have values:
        Enumeration<FileDicomKey> e = tags2save.keys();

        while (e.hasMoreElements()) {
            FileDicomKey tagKey = e.nextElement();

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
    protected void LSMDataConversion(FileInfoLSM sourceInfo, FileInfoImageXML destInfo) {
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
    private static void copyResolutions(ModelImage modelImageSrc, ModelImage modelImageResult, int sliceNum) {
        float[] resolutions = new float[3];
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
    private static float[] resample255(float[] buffer, float min, float max) {
        float precalculatedDenominator = max - min; // precalculated (max - min) for speed

        for (int i = 0; i < buffer.length; i++) {
            buffer[i] = ( (buffer[i] - min) / precalculatedDenominator) * 255;
        }

        return buffer;
    }

    /**
     * Sorts an array of floats (using insertion sort), turns into array of ints used to sort images by slice location;
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
    private static boolean sort(float[] A, int[] B, int size) {
        boolean flag = true;
        int stop = size - 1, i, tmp2;
        float tmp;

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

        int[] C = new int[size];

        for (i = 0; i < size; i++) {
            C[B[i]] = i;
        }

        for (i = 0; i < size; i++) {
            B[i] = C[i];
        }

        return flag;
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
    private static boolean sort(int[] A, int[] B, int size) {
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

        int[] C = new int[size];

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
    private int absBiggest(double zero, double one, double two) {

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
    private boolean callDialog(int[] extents, boolean isTiff, FileWriteOptions options) {
        JDialogSaveSlices dialogSave = null;

        if ( (extents.length == 2) && isTiff && options.isPackBitEnabled()) {
            int response = JOptionPane.showConfirmDialog(UI.getMainFrame(), "Save with pack bit compression?",
                    "Compression", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);

            if (response == JOptionPane.YES_OPTION) {
                options.setWritePackBit(true);
                options.setDefault(false);
            } else {
                options.setWritePackBit(false);
            }
        } else if (extents.length > 2) {

            if (extents.length == 3) {
                dialogSave = new JDialogSaveSlices(UI.getMainFrame(), 0, extents[2] - 1, options);
            } else if (extents.length == 4) {
                dialogSave = new JDialogSaveSlices(UI.getMainFrame(), 0, extents[2] - 1, 0, extents[3] - 1, options);
            }

            if (dialogSave.isCancelled()) {
                return false;
            }

            options = dialogSave.getWriteOptions();

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
    private void createProgressBar(FileBase fBase, String fName, String message) {

        // progressBar = new ViewJProgressBar(fName, message + fName + " ...", 0, 100, true);
        // progressBar.setVisible(ViewUserInterface.getReference().isAppFrameVisible() && !quiet);

        // the quiet flag is needed to determine if progress bar is visible or not
        progressBar = new ViewJProgressBar(fName, message + fName + " ...", 0, 100, true, null, null, !quiet);
        progressBar.progressStateChanged(new ProgressChangeEvent(this, 0, null, null));

        if (fBase != null) {
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
    private ModelImage readAfni(String fileName, String fileDir, boolean loadB) {
        ModelImage image = null;
        FileAfni imageFile;
        boolean doRead = true;

        try {
            imageFile = new FileAfni(fileName, fileDir, loadB, doRead);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage();
            // LUT = imageFile.getModelLUT();
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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
    private ModelImage readAnalyze(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileAnalyze imageFile = null;

        if (FileCheshire.isCheshire(fileName, fileDir)) {
            image = readCheshire(fileName, fileDir);
        } else {

            // most likely an Analyze file
            try {
                imageFile = new FileAnalyze(fileName, fileDir);
                createProgressBar(imageFile, fileName, FILE_READ);
                image = imageFile.readImage(one);
            } catch (IOException error) {

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
            } catch (OutOfMemoryError error) {

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
        }
        imageFile = null;
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
    private ModelImage readAnalyzeMulti(String fileName, String fileDir) {
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

            for (int m = 0; m < fileList.length; m++) {

                if (fileList[m] != null) {

                    // System.out.println(" Name = " + fileList[m]);
                    i++;
                }
            }
        } catch (OutOfMemoryError error) {

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

        createProgressBar(null, fileName, FILE_READ);

        // if one of the images has the wrong extents, the following must be changed.
        // (ei., too many images!)
        // for simplicity of setup, read in the first file hdr
        imageFile = new FileAnalyze(fileList[0], fileDir);

        try {

            if ( !imageFile.readHeader(fileList[0], fileDir)) {
                throw (new IOException(" Analyze header file error"));
            }
        } catch (IOException ioe) {

            if ( !quiet) {
                MipavUtil.displayError("Error reading header file.");
            }

            ioe.printStackTrace();
        }

        fileInfo = ((FileAnalyze) imageFile).getFileInfo();
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

        int[] imgExtents = new int[extents.length + 1];
        float[] imgResolutions = new float[resolutions.length + 1]; // should be same size as extents.

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

                if ( ! ((FileAnalyze) imageFile).readHeader(fileList[i], fileDir)) {
                    throw (new IOException(" Analyze header file error"));
                }

                // chk the extents of the image to verify it is consistent
                // (this doesn't ensure there won't be null exceptions@)
                fileInfo = ((FileAnalyze) imageFile).getFileInfo();

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

                ((FileAnalyze) imageFile).readImage(buffer);
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
            } catch (IOException error) {

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
            } catch (ArrayIndexOutOfBoundsException error) {

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
            } catch (OutOfMemoryError error) {

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
            FileInfoBase[] fileInfoArr = image.getFileInfo();

            imgExtents[image.getNDims() - 1] = imageCount; // last dimension available should be the num images read

            int sliceSize = buffer.length;

            buffer = new float[sliceSize * imageCount];

            try {
                image.exportData(0, buffer.length, buffer); // copy all buffer
                image.reallocate(fileInfo.getDataType(), imgExtents);
                image.importData(0, buffer, true); // remake the model image with the right number of slices.

                for (i = 0; i < imgExtents[imgExtents.length - 1]; i++) { // copy all image info
                    fileInfoArr[i].setExtents(imgExtents); // update extents
                    image.setFileInfo(fileInfoArr[i], i); // copying...
                }
            } catch (IOException ioe) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO reports: " + ioe.getMessage());
                }

                return null;
            }

            FileInfoBase[] fileInfoArrCopy = new FileInfoBase[imgExtents[imgExtents.length - 1]];

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
    private ModelImage readAvi(String fileName, String fileDir, boolean one, boolean readQT) {
        FileAvi imageFile;
        ModelImage image = null;

        try {
            imageFile = new FileAvi(fileName, fileDir);
            imageFile.setReadQT(readQT);
            // imageFile.setProgressBar(pInterface);

            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage(one);

            // LUT used in compressed RLE8 files
            LUT = imageFile.getModelLUT();
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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
    private ModelImage readBioRad(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileBioRad imageFile;

        try {
            imageFile = new FileBioRad(fileName, fileDir);
            image = imageFile.readImage(one);
            // LUT = imageFile.getModelLUT();
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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
    private ModelImage readBRUKER(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileBRUKER imageFile = null;
        FileInfoBase myFileInfo;
        File directoryFile;
        String parentDirectoryName;

        try {
            fileName = "d3proc";
            imageFile = new FileBRUKER(fileName, fileDir); // read in files
            imageFile.readd3proc();
        } catch (IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("d3proc FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("d3proc Out of memory: " + error);
            }

            error.printStackTrace();

            return null;
        }

        ((FileBRUKER) imageFile).setFileName("reco");

        try {
            imageFile.readreco();
        } catch (IOException error) {
            myFileInfo = imageFile.getFileInfo();
            myFileInfo.setEndianess(FileBase.BIG_ENDIAN);
        } catch (OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("reco out of memory: " + error);
            }

            error.printStackTrace();

            return null;
        }

        ((FileBRUKER) imageFile).setFileName("acqp");
        directoryFile = new File(fileDir);

        File tmpFile = new File(fileDir + File.separator + "acqp");

        if ( !tmpFile.exists()) {

            // go up 2 parent directories
            parentDirectoryName = directoryFile.getParent();
            directoryFile = new File(parentDirectoryName);
            ((FileBRUKER) imageFile).setFileDir(directoryFile.getParent() + File.separator);
        } else {
            ((FileBRUKER) imageFile).setFileDir(directoryFile + File.separator);
        }

        try {
            imageFile.readacqp();
        } catch (IOException error) {
            Preferences.debug("IOExceoption in FileIO.readBRUKER\n", Preferences.DEBUG_FILEIO);

            if ( !quiet) {
                MipavUtil.displayError("IOException in FileIO.readBRUKER: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("acqp out of memory: " + error);
            }

            return null;
        }

        ((FileBRUKER) imageFile).setFileName("2dseq");
        ((FileBRUKER) imageFile).setFileDir(fileDir);

        try {
            image = imageFile.readImage(one);
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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
    private ModelImage readCheshire(String fileName, String fileDir) {
        ModelImage image = null;
        FileCheshire imageFile;

        try {
            imageFile = new FileCheshire(fileName, fileDir, true);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage();
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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
     * Reads a COR file by first reading the header file then reading in each separate slice file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readCOR(String fileName, String fileDir) {
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
        } catch (IOException error) {
            tryAgain = true;
        } catch (OutOfMemoryError error) {

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
            } catch (IOException error) {
                tryAgain = true;
            } catch (OutOfMemoryError error) {

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
            } catch (IOException error) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                error.printStackTrace();

                return null;
            } catch (OutOfMemoryError error) {

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

            // Progress bar shows what % of images have been read.
            createProgressBar(null, fileName, FILE_READ);

        } catch (OutOfMemoryError error) {

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
        } catch (IOException error) {

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
        for (i = 0; i < nImages; i++) {

            try {

                // progressBar.setTitle(UI.getProgressBarPrefix() + "image " + fileList[i]);
                progressBar.updateValueImmed(Math.round((float) i / (nImages - 1) * 100));
                ((FileCOR) imageFile).setFileName(fileList[i]);
                ((FileCOR) imageFile).readImage(length);
                image.setFileInfo(myFileInfo, i);

                if (image.isColorImage()) {
                    image.importData(i * 4 * length, ((FileCOR) imageFile).getImageBuffer(), false);
                } else {
                    image.importData(i * length, ((FileCOR) imageFile).getImageBuffer(), false);
                }
            } catch (IOException error) {

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
            } catch (ArrayIndexOutOfBoundsException error) {

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
            } catch (OutOfMemoryError error) {

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
    private ModelImage readDM3(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileDM3 imageFile;

        try {
            imageFile = new FileDM3(fileName, fileDir);
            image = imageFile.readImage(one);
            // LUT = imageFile.getModelLUT();
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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
    private ModelImage readFits(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileFits imageFile;

        try {
            imageFile = new FileFits(fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage(one);
            // LUT = imageFile.getModelLUT();
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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
     * Reads in a GE Genesis 5x type file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readGEGenesis5X(String fileName, String fileDir) {

        ModelImage image = null;
        FileGESigna5X imageFile;
        FileInfoBase myFileInfo;
        FileInfoBase myFileInfo0 = null;
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

            createProgressBar(null, fileName, FILE_READ);

            width = imageFile.getWidth();
            height = imageFile.getHeight();
            length = width * height;
            buffer = new float[length];

            if ( (fileList.length == 1) || (imageFile.getStartAdjust() > 0)) {
                extents = new int[2];
                extents[0] = width;
                extents[1] = height;
            } else {
                extents = new int[3];
                extents[0] = width;
                extents[1] = height;
                extents[2] = fileList.length;
            }

            image = new ModelImage(ModelImage.USHORT, extents, "GE");
        } catch (OutOfMemoryError error) {

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
        } catch (IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            if (progressBar != null) {}

            return null;
        }

        if (imageFile.getStartAdjust() > 0) {
            nImages = 1;
        } else {
            nImages = fileList.length;
        }

        // loop through files, place them in image array
        for (i = 0; i < nImages; i++) {

            try {
                progressBar.setTitle(UI.getProgressBarPrefix() + "image " + fileList[i]);
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
                                slice0Pos = ((FileInfoGESigna5X) myFileInfo).imgTLHC_S;
                                break;

                            case FileInfoBase.CORONAL:
                                slice0Pos = ((FileInfoGESigna5X) myFileInfo).imgTLHC_A;
                                break;

                            case FileInfoBase.SAGITTAL:
                                slice0Pos = ((FileInfoGESigna5X) myFileInfo).imgTLHC_R;
                                break;
                        }
                    } // if (i == 0)
                    else if (i == 1) {
                        orient = myFileInfo.getAxisOrientation();

                        switch (myFileInfo.getImageOrientation()) {

                            case FileInfoBase.AXIAL:
                                slice1Pos = ((FileInfoGESigna5X) myFileInfo).imgTLHC_S;
                                if (slice1Pos > slice0Pos) {
                                    orient[2] = FileInfoBase.ORI_I2S_TYPE;
                                } else {
                                    orient[2] = FileInfoBase.ORI_S2I_TYPE;
                                }

                                break;

                            case FileInfoBase.CORONAL:
                                slice1Pos = ((FileInfoGESigna5X) myFileInfo).imgTLHC_A;
                                if (slice1Pos > slice0Pos) {
                                    orient[2] = FileInfoBase.ORI_P2A_TYPE;
                                } else {
                                    orient[2] = FileInfoBase.ORI_A2P_TYPE;
                                }

                                break;

                            case FileInfoBase.SAGITTAL:
                                slice1Pos = ((FileInfoGESigna5X) myFileInfo).imgTLHC_R;
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
                    myFileInfo.setOrigin( ((FileInfoGESigna5X) (myFileInfo)).getOriginAtSlice(imageFile
                            .getImageNumber() - 1));
                    image.setFileInfo(myFileInfo, imageFile.getImageNumber() - 1);
                    image.importData( (imageFile.getImageNumber() - 1) * length, buffer, false);
                } // if (fileList[i] != null)
            } // try
            catch (IOException error) {

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
            } catch (OutOfMemoryError error) {

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

        return image;
    }

    /**
     * Reads in a GE Signa 4x type file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readGESigna4X(String fileName, String fileDir) {

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

            image = new ModelImage(ModelImage.USHORT, extents, "GE");
        } catch (OutOfMemoryError error) {

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
        } catch (IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        nImages = fileList.length;

        createProgressBar(null, fileName, FILE_READ);

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
            catch (IOException error) {

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
            } catch (OutOfMemoryError error) {

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
    private ModelImage readICS(String fileName, String fileDir) {
        ModelImage image = null;
        FileICS imageFile;

        try {
            imageFile = new FileICS(fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage();
            // LUT = imageFile.getModelLUT();
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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
    private ModelImage readInterfile(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileInterfile imageFile;

        try {
            imageFile = new FileInterfile(fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage(one);
            // LUT = imageFile.getModelLUT();
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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
    private ModelImage readJimi(String fileName, String fileDir, boolean multifile) {
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

        MediaTracker mediaTracker = new MediaTracker(UI.getMainFrame());
        extents = new int[] {0, 0, fileList.length};

        for (int j = 0; j < fileList.length; j++) {

            try {

                mediaTracker.addImage(image, 0);

                boolean loaded = mediaTracker.waitForAll(20000);

                if ( !loaded || (image == null)) {

                    try {
                        image = (Image) ImageIO.read(new File(fileDir + fileList[j])); // if JIMI fails, try this

                        // String[] readTypes = ImageIO.getReaderFormatNames();

                        // for (int t = 0; t < readTypes.length; t++) {
                        // System.out.println("ImageIO read formats: " + readTypes[t]);
                        // }

                        // String[] writeTypes = ImageIO.getWriterFormatNames();

                        // for (int t = 0; t < writeTypes.length; t++) {
                        // System.out.println("ImageIO write formats: " + writeTypes[t]);
                        // }

                    } catch (IOException ioe) {
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
            } catch (InterruptedException e) {

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
            int[] pixels = new int[imageWidth * imageHeight];
            PixelGrabber pg = new PixelGrabber(image, 0, 0, imageWidth, imageHeight, pixels, 0, imageWidth);

            try {
                pg.grabPixels();
            } catch (InterruptedException e) {
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
                if ( !isPaintBrush) {
                    greyBuffer = new int[extents[0] * extents[1] * extents[2]];
                }
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
            int[] tmp = new int[] {extents[0], extents[1]};

            extents = new int[] {tmp[0], tmp[1]};
        }

        // need to determine if the buffer identifies a grey scale image...but disregard if we
        // are dealing with the paintbrush
        boolean isGrey = false;
        if ( !isPaintBrush) {
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
        } catch (IOException e) {
            Preferences.debug("FileIO.JIMI : " + e + "\n", Preferences.DEBUG_FILEIO);
            e.printStackTrace();
        }

        // get the fileInfo and make sure some fields are set (like the
        // fileDir and the fileFormat
        FileInfoBase[] fileInfo = modelImage.getFileInfo();

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
    private ModelImage readLSM(String fileName, String fileDir, int secondAddress, boolean one) {
        ModelImage image = null;
        FileLSM imageFile;

        try {
            imageFile = new FileLSM(fileName, fileDir, secondAddress);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage(false, one);
            LUT = imageFile.getModelLUT();
            secondImage = imageFile.getSecondImage();
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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
     * Reads a multi LSM file by first reading the headers then reading in each separate file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readLSMMulti(String fileName, String fileDir) {
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
        int secondAddress = 0;
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
        } catch (IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (OutOfMemoryError error) {

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
                    unitsOfMeasure[3] = FileInfoBase.SECONDS;
                    unitsOfMeasure[2] = singleUnitsOfMeasure[2];
                } else {
                    extents = new int[3];
                    extents[2] = nImages;
                    unitsOfMeasure = new int[3];
                    unitsOfMeasure[2] = FileInfoBase.SECONDS;
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
                    FILE_READ);

        } catch (OutOfMemoryError error) {

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
            imageFile = new FileLSM(fileList[0], fileDir, secondAddress);
        } catch (IOException error) {

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
                    ((FileLSM) imageFile).setFileName(fileList[i]);

                    // fileLSM.testme(i);
                    ((FileLSM) imageFile).readImage(true, false);
                    myFileInfo = ((FileLSM) imageFile).getFileInfo();
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
                        image.importData(i * 4 * length, ((FileLSM) imageFile).getImageBuffer(), false);
                    } else {
                        image.importData(i * length, ((FileLSM) imageFile).getImageBuffer(), false);
                    }

                } catch (IOException error) {

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
                } catch (ArrayIndexOutOfBoundsException error) {

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
                } catch (OutOfMemoryError error) {

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
                    ((FileLSM) imageFile).setFileName(fileList[i]);

                    // fileLSM.testme(i);
                    ((FileLSM) imageFile).readImage(true, false);
                    myFileInfo = ((FileLSM) imageFile).getFileInfo();
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
                            image.importData( (i * 4 * length) + (j * 4 * extents[0] * extents[1]),
                                    ((FileLSM) imageFile).getImage3DMultiBuffer()[j], false);
                        } else {
                            image.importData( (i * length) + (j * extents[0] * extents[1]), ((FileLSM) imageFile)
                                    .getImage3DMultiBuffer()[j], false);
                        }
                    } // for (j = 0; j < extents[2]; j++) {
                } catch (IOException error) {

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
                } catch (ArrayIndexOutOfBoundsException error) {

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
                } catch (OutOfMemoryError error) {

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

        return image;
    }

    /**
     * Reads a Magnetom Vision file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param one Indicates that only the named file should be read, as opposed to reading the matching files in the
     *            directory, as defined by the filetype. <code>true</code> if only want to read one image from 3D
     *            dataset.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readMagnetomVision(String fileName, String fileDir, boolean one) {
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

        if (one) {
            return readOneMagnetomVision(fileName, fileDir);
        }

        try {
            fileList = FileUtility.getFileList(fileDir, fileName, quiet);
            imageFile = new FileMagnetomVision(fileName, fileDir);

            imageFile.setFileName(fileList[0]);

            createProgressBar(null, fileName, FILE_READ);

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

            sort(imageNumbers, indicies, nImages);
        } catch (OutOfMemoryError error) {

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
        } catch (IOException error) {

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
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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
    private ModelImage readMap(String fileName, String fileDir) {
        ModelImage image = null;
        FileMap imageFile;
        FileInfoBase fileInfo;
        int i;

        JDialogRawIO mapIODialog = new JDialogRawIO(UI.getMainFrame(), "MAP");

        mapIODialog.setVisible(true);

        if (mapIODialog.isCancelled() == true) {
            return null;
        }

        try {
            fileInfo = new FileInfoImageXML(fileName, fileDir, FileUtility.RAW);
            image = new ModelImage(mapIODialog.getDataType(), mapIODialog.getExtents(), fileName);
        } catch (OutOfMemoryError error) {

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
            createProgressBar(imageFile, fileName, FILE_READ);
            imageFile.readImage(image, mapIODialog.getOffset());
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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
    private ModelImage readMedVision(String fileName, String fileDir) {
        ModelImage image = null;
        FileMedVision imageFile;

        try {
            imageFile = new FileMedVision(fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage();
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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
    private ModelImage readMGH(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileMGH imageFile;

        try {
            imageFile = new FileMGH(fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage(one);
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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
    private ModelImage readMicroCat(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileMicroCat imageFile;
        FileInfoMicroCat fileInfoMicro;

        try {
            imageFile = new FileMicroCat(fileName, fileDir);

            if (fileName.endsWith(".ct")) {
                int i;

                File imageDir = new File(fileDir);
                String[] fileList = imageDir.list();

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
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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
    private ModelImage readMinc(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileMinc imageFile;

        try {
            imageFile = new FileMinc(fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage(one);
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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

    private ModelImage readMincHDF(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileMincHDF imageFile;

        try {
            imageFile = new FileMincHDF(fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage(one);
        } catch (Exception error) {

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
        } catch (OutOfMemoryError error) {

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
     * Reads a MRC file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readMRC(String fileName, String fileDir) {
        ModelImage image = null;
        FileMRC imageFile;

        try {
            imageFile = new FileMRC(fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage();
            // LUT = imageFile.getModelLUT();
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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
     * Reads a BFLOAT file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param one If true, read in only one middle slice
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readBFLOAT(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileBFLOAT imageFile;

        try {
            imageFile = new FileBFLOAT(fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage(one);
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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
    private ModelImage readNIFTI(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileNIFTI imageFile;

        try {
            imageFile = new FileNIFTI(fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage(one);
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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
    private ModelImage readNIFTIMulti(String fileName, String fileDir) {
        ModelImage image = null;
        FileNIFTI imageFile;
        FileInfoNIFTI fileInfo;

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

            for (int m = 0; m < fileList.length; m++) {

                if (fileList[m] != null) {

                    // System.out.println(" Name = " + fileList[m]);
                    i++;
                }
            }
        } catch (OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        nImages = i; // total number of suspected files to import into an image

        if (nImages == 1) {
            return readNIFTI(fileName, fileDir, false);
        }

        createProgressBar(null, FileUtility.trimNumbersAndSpecial(fileName) + FileUtility.getExtension(fileName),
                FILE_READ);

        // System.out.println("nImage = " + i);
        // System.out.println(" filelist[0] = " + fileList[0]);
        // System.out.println(" filelist[1] = " + fileList[1]);
        // if one of the images has the wrong extents, the following must be changed.
        // (ei., too many images!)
        // for simplicity of setup, read in the first file hdr
        imageFile = new FileNIFTI(fileList[0], fileDir);

        try {

            if ( !imageFile.readHeader(fileList[0], fileDir)) {
                throw (new IOException(" NIFTI header file error"));
            }
        } catch (IOException ioe) {

            if ( !quiet) {
                MipavUtil.displayError("Error reading header file.");
            }

            ioe.printStackTrace();
        }

        fileInfo = ((FileNIFTI) imageFile).getFileInfo();
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

        int[] imgExtents = new int[extents.length + 1];
        float[] imgResolutions = new float[resolutions.length + 1]; // should be same size as extents.

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

                if ( ! ((FileNIFTI) imageFile).readHeader(fileList[i], fileDir)) {
                    throw (new IOException(" NIFTI header file error"));
                }

                // chk the extents of the image to verify it is consistent
                // (this doesn't ensure there won't be null exceptions@)
                fileInfo = ((FileNIFTI) imageFile).getFileInfo();

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

                ((FileNIFTI) imageFile).readImage(buffer);
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
            } catch (IOException error) {

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
            } catch (ArrayIndexOutOfBoundsException error) {

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
            } catch (OutOfMemoryError error) {

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
            FileInfoBase[] fileInfoArr = image.getFileInfo();

            imgExtents[image.getNDims() - 1] = imageCount; // last dimension available should be the num images read

            int sliceSize = buffer.length;

            buffer = new float[sliceSize * imageCount];

            try {
                image.exportData(0, buffer.length, buffer); // copy all buffer
                image.reallocate(fileInfo.getDataType(), imgExtents);
                image.importData(0, buffer, true); // remake the model image with the right number of slices.

                for (i = 0; i < imgExtents[imgExtents.length - 1]; i++) { // copy all image info
                    fileInfoArr[i].setExtents(imgExtents); // update extents
                    image.setFileInfo(fileInfoArr[i], i); // copying...
                }
            } catch (IOException ioe) {

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
    private ModelImage readNRRD(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileNRRD imageFile;

        try {
            imageFile = new FileNRRD(fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage(one);
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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
     * Reads in a single GE Genesis type file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readOneGEGenesis5X(String fileName, String fileDir) {
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

            image = new ModelImage(ModelImage.USHORT, extents, "GE");
            imageFile.readImage(buffer);
            myFileInfo = imageFile.getFileInfo();
            myFileInfo.setExtents(extents);
            image.setFileInfo(myFileInfo, 0);
            image.setImageName(imageFile.getFileInfo().getImageNameFromInfo(), false);
            image.importData(0, buffer, false);
        } catch (OutOfMemoryError error) {

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
        } catch (IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

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
    private ModelImage readOneGESigna4X(String fileName, String fileDir) {
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

            image = new ModelImage(ModelImage.USHORT, extents, "GE");
            imageFile.readImage(buffer);
            myFileInfo = imageFile.getFileInfo();
            myFileInfo.setExtents(extents);
            image.setFileInfo(myFileInfo, 0);
            image.setImageName(imageFile.getFileInfo().getImageNameFromInfo(), false);
            image.importData(0, buffer, false);
        } catch (OutOfMemoryError error) {

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
        } catch (IOException error) {

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
    private ModelImage readOneMagnetomVision(String fileName, String fileDir) {
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
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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

        return image;
    }

    /**
     * Reads an OSM file by calling the read method of the file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readOSM(String fileName, String fileDir) {
        ModelImage image = null;
        FileOSM imageFile;

        try {
            imageFile = new FileOSM(fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage();
            // LUT = imageFile.getModelLUT();
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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
    private ModelImage readPARREC(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FilePARREC imageFile;

        try {
            imageFile = new FilePARREC(fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage(one);
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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
    private ModelImage readQT(String fileName, String fileDir) {

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
    private ModelImage readRaw(String fileName, String fileDir, FileInfoBase fileInfo) {
        ModelImage image = null;
        FileRaw imageFile;
        int i;

        if (fileInfo == null) {
            fileInfo = new FileInfoImageXML(fileName, fileDir, FileUtility.RAW);

            if (rawInfo == null) {
                JDialogRawIO rawIODialog = new JDialogRawIO(UI.getMainFrame(), "Raw");

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
        } catch (OutOfMemoryError error) {

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
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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
    private ModelImage readRawMulti(String fileName, String fileDir, FileInfoBase fileInfo) {
        ModelImage image = null;
        FileRaw imageFile;
        FileInfoImageXML[] nFileInfos;
        String[] fileList;
        int i;

        int nImages;

        if (fileInfo == null) {
            JDialogRawIO rawIODialog = new JDialogRawIO(UI.getMainFrame(), "Raw");

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
        } catch (OutOfMemoryError error) {

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

        createProgressBar(null, fileName, FILE_READ);

        nFileInfos = new FileInfoImageXML[nImages];

        if (nImages > 1) {
            int[] extents = new int[3];

            extents[0] = fileInfo.getExtents()[0];
            extents[1] = fileInfo.getExtents()[1];
            extents[2] = nImages;
            fileInfo.setExtents(extents);

            float[] resols = new float[3];
            resols[0] = fileInfo.getResolution(0);
            resols[1] = fileInfo.getResolution(1);
            resols[2] = fileInfo.getResolution(2);
            fileInfo.setResolutions(resols);
        }

        int length = fileInfo.getExtents()[0] * fileInfo.getExtents()[1];
        float[] buffer;

        try {
            buffer = new float[length];
            image = new ModelImage(fileInfo.getDataType(), fileInfo.getExtents(), fileName);
        } catch (OutOfMemoryError error) {

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
            } catch (IOException error) {

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
            } catch (OutOfMemoryError error) {

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
    private ModelImage readSPM(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileSPM imageFile;

        try {
            imageFile = new FileSPM(fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage(one);
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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
    private ModelImage readSTK(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileSTK imageFile;

        try {
            imageFile = new FileSTK(fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage(false, one);
            LUT = imageFile.getModelLUT();
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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
    private ModelImage readLIFF(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileLIFF imageFile;

        try {
            imageFile = new FileLIFF(fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage(false, one);
            LUT = imageFile.getModelLUT();

        } catch (IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

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
    private ModelImage readTiff(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileTiff imageFile;

        try {
            imageFile = new FileTiff(fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage(false, one);
            LUT = imageFile.getModelLUT();

        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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
     * Reads a multi TIFF file by first reading the headers then reading in each separate file.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * 
     * @return The image that was read in, or null if failure.
     */
    private ModelImage readTiffMulti(String fileName, String fileDir) {
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

                return image;
            } else {
                imageFile.readImage(true, false);
            }
        } catch (IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        } catch (OutOfMemoryError error) {

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
                    FILE_READ);

        } catch (OutOfMemoryError error) {

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

        try {
            imageFile = new FileTiff(fileList[0], fileDir);
        } catch (IOException error) {

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
                ((FileTiff) imageFile).setFileName(fileList[i]);

                // fileTIFF.testme(i);
                ((FileTiff) imageFile).readImage(true, false);
                myFileInfo = ((FileTiff) imageFile).getFileInfo();
                myFileInfo.setExtents(extents);
                myFileInfo.setResolutions(resols);

                if (nFiles > 1) {
                    myFileInfo.setMultiFile(true);
                }

                image.setFileInfo(myFileInfo, i);

                // float[] tmpBuffer = fileTIFF.getImageBuffer();
                if (image.isColorImage()) {
                    image.importData(i * 4 * length, ((FileTiff) imageFile).getImageBuffer(), false);
                } else {
                    image.importData(i * length, ((FileTiff) imageFile).getImageBuffer(), false);
                }
            } catch (IOException error) {

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
            } catch (ArrayIndexOutOfBoundsException error) {

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
            } catch (OutOfMemoryError error) {

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
    private ModelImage readTMG(String fileName, String fileDir) {
        ModelImage image = null;
        FileTMG imageFile;

        try {
            imageFile = new FileTMG(fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage();
            // LUT = imageFile.getModelLUT();
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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
    private ModelImage readXML(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileImageXML imageFile;
        // don't show splash screen:

        try {
            imageFile = new FileImageXML(fileName, fileDir);

            if ( ! (fileName.equals("splash.xml") || (one == true))) {
                createProgressBar(imageFile, fileName, FILE_READ);
            }

            image = imageFile.readImage(one);
            LUT = imageFile.getModelLUT();
            modelRGB = imageFile.getModelRGB();
        } catch (IOException error) {

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
        } catch (OutOfMemoryError error) {

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

        // System.out.println(" image = " + image);
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
    private ModelImage readXMLMulti(String fileName, String fileDir) {
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

            for (int m = 0; m < fileList.length; m++) {

                if (fileList[m] != null) {

                    // System.out.println(" Name = " + fileList[m]);
                    i++;
                }
            }
        } catch (OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return null;
        }

        nImages = i; // total number of suspected files to import into an image

        if (nImages == 1) {
            return readXML(fileName, fileDir, false);
        }

        createProgressBar(null, FileUtility.trimNumbersAndSpecial(fileName) + FileUtility.getExtension(fileName),
                FILE_READ);

        // System.out.println(" filelist[1] = " + fileList[1]);
        // if one of the images has the wrong extents, the following must be changed.
        // (ei., too many images!)
        // for simplicity of setup, read in the first file hdr
        float[][] res = null;

        imageFile = new FileImageXML(fileList[0], fileDir);

        try {
            TalairachTransformInfo talairach = new TalairachTransformInfo();
            res = imageFile.readHeader(fileList[0], fileDir, talairach);

            if (res == null) {
                throw (new IOException(" Analyze header file error"));
            }
        } catch (IOException ioe) {

            if ( !quiet) {
                MipavUtil.displayError("Error reading header file.");
            }

            ioe.printStackTrace();
        }

        fileInfo = ((FileImageXML) imageFile).getFileInfo();
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

        int[] imgExtents = new int[extents.length + 1];
        float[] imgResolutions = new float[resolutions.length + 1]; // should be same size as extents.

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

                TalairachTransformInfo talairach = new TalairachTransformInfo();
                res = ((FileImageXML) imageFile).readHeader(fileList[i], fileDir, talairach);

                if (res == null) {
                    throw (new IOException(" XML header file error"));
                }

                // chk the extents of the image to verify it is consistent
                // (this doesn't ensure there won't be null exceptions@)
                fileInfo = ((FileImageXML) imageFile).getFileInfo();

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

                ((FileImageXML) imageFile).readImage(buffer);
                image.importData(imageCount * length, buffer, false);
                image.setFileInfo(fileInfo, imageCount);
                imageCount++; // image was okay, so count it.(can't do it before b/c of offset)

            } catch (IOException error) {

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
            } catch (ArrayIndexOutOfBoundsException error) {

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
            } catch (OutOfMemoryError error) {

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
            FileInfoBase[] fileInfoArr = image.getFileInfo();

            imgExtents[image.getNDims() - 1] = imageCount; // last dimension available should be the num images read

            int sliceSize = buffer.length;

            buffer = new float[sliceSize * imageCount];

            try {
                image.exportData(0, buffer.length, buffer); // copy all buffer
                image.reallocate(fileInfo.getDataType(), imgExtents);
                image.importData(0, buffer, true); // remake the model image with the right number of slices.

                for (i = 0; i < imgExtents[imgExtents.length - 1]; i++) { // copy all image info
                    fileInfoArr[i].setExtents(imgExtents); // update extents
                    image.setFileInfo(fileInfoArr[i], i); // copying...
                }
            } catch (IOException ioe) {

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
    private boolean writeAfni(ModelImage image, FileWriteOptions options) {
        FileAfni afniFile;

        try { // Construct a new file object

            if (image.getNDims() < 3) {
                MipavUtil.displayError("Error! Image must have 3 or 4 dimensions");

                return false;
            }

            boolean loadB = false;
            boolean doRead = false;

            afniFile = new FileAfni(options.getFileName(), options.getFileDirectory(), loadB, doRead);
            createProgressBar(afniFile, options.getFileName(), FILE_WRITE);
            afniFile.writeImage(image, options);
            afniFile.finalize();
            afniFile = null;
        } catch (IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (OutOfMemoryError error) {

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
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeAnalyze(ModelImage image, FileWriteOptions options) {
        FileAnalyze analyzeFile;

        if (Preferences.is(Preferences.PREF_SAVE_XML_ON_HDR_SAVE)) {
            FileImageXML xmlFile;

            try {
                xmlFile = new FileImageXML(options.getFileName(), options.getFileDirectory());

                String fBase, fName = options.getFileName();
                int index = fName.lastIndexOf(".");

                if (index != -1) {
                    fBase = fName.substring(0, index);
                } else {
                    fBase = fName.substring(0);
                }

                xmlFile.setRawExtension(".img");
                xmlFile.writeHeader(image, options, fBase, options.getFileDirectory(), false);
            } catch (IOException error) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                error.printStackTrace();

                return false;
            } catch (OutOfMemoryError error) {

                if ( !quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                error.printStackTrace();

                return false;
            }
        }

        try { // Construct a new file object
            analyzeFile = new FileAnalyze(options.getFileName(), options.getFileDirectory());
            createProgressBar(analyzeFile, options.getFileName(), FILE_WRITE);
            analyzeFile.writeImage(image, options);
            analyzeFile.finalize();
            analyzeFile = null;
        } catch (IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (OutOfMemoryError error) {

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
    private boolean writeAvi(ModelImage image, FileWriteOptions options) {

        String fileName = options.getFileName();

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

        } catch (IOException ex) {
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
    private boolean writeCOR(ModelImage image, FileWriteOptions options) {
        FileCOR corFile;

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

            if ( (image.getFileInfo(0).getResolution(0) != 1.0f)
                    || ( (image.getFileInfo(0).getUnitsOfMeasure()[0] != FileInfoBase.MILLIMETERS) && (image
                            .getFileInfo(0).getUnitsOfMeasure()[0] != FileInfoBase.UNKNOWN_MEASURE))) {
                MipavUtil.displayError("Error! x resolution must be 1.0 millimeter");

                return false;
            }

            if ( (image.getFileInfo(0).getResolution(1) != 1.0f)
                    || ( (image.getFileInfo(0).getUnitsOfMeasure()[1] != FileInfoBase.MILLIMETERS) && (image
                            .getFileInfo(0).getUnitsOfMeasure()[1] != FileInfoBase.UNKNOWN_MEASURE))) {
                MipavUtil.displayError("Error! y resolution must be 1.0 millimeter");

                return false;
            }

            if ( (image.getFileInfo(0).getResolution(2) != 1.0f)
                    || ( (image.getFileInfo(0).getUnitsOfMeasure()[2] != FileInfoBase.MILLIMETERS) && (image
                            .getFileInfo(0).getUnitsOfMeasure()[2] != FileInfoBase.UNKNOWN_MEASURE))) {
                MipavUtil.displayError("Error! z resolution must be 1.0 millimeter");

                return false;
            }

            corFile = new FileCOR(options.getFileName(), options.getFileDirectory());
            corFile.writeImage(image, options);
        } catch (IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
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
    private boolean writeDicom(ModelImage image, FileWriteOptions options) {
        int i;
        int index;
        String prefix = "";
        String fileSuffix = "";
        FileInfoDicom myFileInfo = null;
        FileInfoBase[] originalFileInfos;
        FileDicom dicomFile = null;
        String fileDir = null;
        String fileName = null;

        // if a file is being 'saved as' a dicom file, then
        // actually save it to a subdirectory, named by the base of the FileName
        fileName = options.getFileName();

        if ( /* options.isSaveAs() && */options.isSaveInSubdirectory()) {
            String baseName = null;

            // find the root of the filename (without extensions)
            int ind = options.getFileName().lastIndexOf(".");

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
        File tmpFile = new File(fileDir);

        if ( !tmpFile.exists()) {

            try {
                tmpFile.mkdirs();
                // don't reset here...may need for options to hold a root if we are saving into subdirs.
                // options.setFileDirectory(fileDir);
            } catch (Exception e) {
                MipavUtil.displayError("Unable to create directory for DICOM file: \n" + fileDir);

                e.printStackTrace();

                return false;
            }
        }

        originalFileInfos = (FileInfoBase[]) (image.getFileInfo().clone());

        int sliceSize = image.getSliceSize();

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
                myFileInfo.getTagTable().setValue("0002,0010", DICOM_Constants.UID_TransferLITTLEENDIANEXPLICIT);
            }

        } else { // Non DICOM images
            myFileInfo = new FileInfoDicom(options.getFileName(), fileDir, FileUtility.DICOM);

            JDialogSaveDicom dialog = new JDialogSaveDicom(UI.getMainFrame(), image.getFileInfo(0), myFileInfo, options
                    .isScript());

            if (dialog.isCancelled()) {
                return false;
            }

            myFileInfo.setEndianess(FileBase.LITTLE_ENDIAN);
            myFileInfo.setRescaleIntercept(0);
            myFileInfo.setRescaleSlope(1);
            myFileInfo.getTagTable().setValue("0002,0010", DICOM_Constants.UID_TransferLITTLEENDIANEXPLICIT);
            myFileInfo.vr_type = FileInfoDicom.EXPLICIT;

            // necessary to save (non-pet) floating point minc files to dicom
            if ( ( (image.getFileInfo(0).getFileFormat() == FileUtility.MINC) && (image.getType() == ModelImage.FLOAT) && (myFileInfo
                    .getModality() != FileInfoBase.POSITRON_EMISSION_TOMOGRAPHY))
                    || ( (image.getFileInfo(0).getFileFormat() == FileUtility.ANALYZE) && (image.getType() == ModelImage.FLOAT))) {
                ModelImage newImage = (ModelImage) image.clone();
                int newType;
                if(newImage.getMin() >= 0) {
                	newType = ModelImage.USHORT;
                }else {
                	newType = ModelImage.SHORT;
                }
                // in-place conversion is required so that the minc file info is retained
                AlgorithmChangeType convertType = new AlgorithmChangeType(newImage, newType,
                        newImage.getMin(), newImage.getMax(), newImage.getMin(), newImage.getMax(), false);
                convertType.run();

                image = newImage;
            }

            if ( (image.getType() == ModelImage.SHORT) || (image.getType() == ModelImage.USHORT)
                    || (image.getFileInfo(0).getDataType() == ModelImage.SHORT)
                    || (image.getFileInfo(0).getDataType() == ModelImage.USHORT)) {
                myFileInfo.getTagTable().setValue("0028,0100", new Short((short) 16), 2);
                myFileInfo.getTagTable().setValue("0028,0101", new Short((short) 16), 2);
                myFileInfo.getTagTable().setValue("0028,0102", new Short((short) 15), 2);
                myFileInfo.getTagTable().setValue("0028,0002", new Short((short) 1), 2); // samples per pixel
                myFileInfo.getTagTable().setValue("0028,0004", new String("MONOCHROME2"), 11); // photometric

                if ( (image.getType() == ModelImage.USHORT)
                        || (image.getFileInfo(0).getDataType() == ModelImage.USHORT)) {
                    myFileInfo.getTagTable().setValue("0028,0103", new Short((short) 0), 2);
                } else {
                    myFileInfo.getTagTable().setValue("0028,0103", new Short((short) 1), 2);
                }
            } else if ( (image.getType() == ModelImage.BYTE) || (image.getType() == ModelImage.UBYTE)
                    || (image.getFileInfo(0).getDataType() == ModelImage.BYTE)
                    || (image.getFileInfo(0).getDataType() == ModelImage.UBYTE)) {
                myFileInfo.getTagTable().setValue("0028,0100", new Short((short) 8), 2);
                myFileInfo.getTagTable().setValue("0028,0101", new Short((short) 8), 2);
                myFileInfo.getTagTable().setValue("0028,0102", new Short((short) 7), 2);
                myFileInfo.getTagTable().setValue("0028,0002", new Short((short) 1), 2); // samples per pixel
                myFileInfo.getTagTable().setValue("0028,0004", new String("MONOCHROME2")); // photometric

                if ( (image.getType() == ModelImage.UBYTE) || (image.getFileInfo(0).getDataType() == ModelImage.UBYTE)) {
                    myFileInfo.getTagTable().setValue("0028,0103", new Short((short) 0), 2);
                } else {
                    myFileInfo.getTagTable().setValue("0028,0103", new Short((short) 1), 2);
                }
            } else if (image.isColorImage()) {
                myFileInfo.getTagTable().setValue("0028,0100", new Short((short) 8), 2);
                myFileInfo.getTagTable().setValue("0028,0101", new Short((short) 8), 2);
                myFileInfo.getTagTable().setValue("0028,0102", new Short((short) 7), 2);
                myFileInfo.getTagTable().setValue("0028,0002", new Short((short) 3), 2); // samples per pixel
                myFileInfo.getTagTable().setValue("0028,0004", new String("RGB")); // photometric
                myFileInfo.getTagTable().setValue("0028,0006", new Short((short) 0), 2); // planar Config
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

            double sliceResolution = myFileInfo.getResolution(2);

            if (image.getExtents().length > 2) { // This sets the fileinfo to the same for all slices !!

                FileInfoBase[] fBase = new FileInfoBase[image.getExtents()[2]];

                double[] axialOrigin = new double[3];
                double[] dicomOrigin = new double[3];
                TransMatrix matrix = myFileInfo.getPatientOrientation();

                if (matrix == null) {
                    matrix = image.getMatrix();
                }

                TransMatrix invMatrix = (TransMatrix) matrix.clone();
                invMatrix.invert();

                float[] imageOrg = image.getFileInfo(0).getOrigin();
                double[] imageOrgDbl = new double[imageOrg.length];

                for (int k = 0; k < imageOrg.length; k++) {
                    imageOrgDbl[k] = (double) imageOrg[k];
                }

                matrix.transform(imageOrgDbl, axialOrigin);

                slLoc = axialOrigin[2];

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

                float[] sliceData = new float[sliceSize];

                for (int k = 0; k < image.getExtents()[2]; k++) {

                    // System.err.println("FileIO k = " + k);
                    fBase[k] = (FileInfoBase) myFileInfo.clone();

                    // Add code to modify the slice location attribute (0020, 1041) VR = DS = decimal string
                    ((FileInfoDicom) (fBase[k])).getTagTable().setValue("0020,1041", Double.toString(slLoc),
                            Double.toString(slLoc).length());
                    slLoc += sliceResolution;

                    // transform the slice position back into dicom space and store it in the file info
                    invMatrix.transform(axialOrigin, dicomOrigin);

                    String tmpStr = new String(Float.toString((float) dicomOrigin[0]) + "\\"
                            + Float.toString((float) dicomOrigin[1]) + "\\" + Float.toString((float) dicomOrigin[2]));
                    ((FileInfoDicom) (fBase[k])).getTagTable().setValue("0020,0032", tmpStr, tmpStr.length());

                    // move the slice position to the next slice in the image
                    axialOrigin[2] += sliceResolution;

                    if (baseInstanceNumber != -1) {
                        String instanceStr = "" + (baseInstanceNumber + k);
                        ((FileInfoDicom) (fBase[k])).getTagTable().setValue("0020,0013", instanceStr,
                                instanceStr.length());
                    }

                    // rescaling intercepts and slopes for each slice.
                    if ( (fBase[k].getModality() == FileInfoBase.POSITRON_EMISSION_TOMOGRAPHY)
                            && ( (image.getType() == ModelStorageBase.FLOAT) || (image.getType() == ModelStorageBase.DOUBLE))) {

                        double smin, smax; // slice min and max

                        try {
                            image.exportData(k * sliceSize, sliceSize, sliceData);
                        } catch (IOException ioe) {
                            image.setFileInfo(originalFileInfos);

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
                        for (int pixel = 0; pixel < sliceData.length; pixel++) {

                            if (sliceData[pixel] < smin) {
                                smin = sliceData[pixel];
                            }

                            if (sliceData[pixel] > smax) {
                                smax = sliceData[pixel];
                            }
                        }

                        fBase[k].setRescaleSlope( (smax - smin) / slopeDivisor);
                        fBase[k].setRescaleIntercept(smin - (fBase[k].getRescaleSlope() * vmin));

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

        createProgressBar(null, options.getFileName(), FILE_WRITE);

        if (options.isSaveAs()) {
            index = options.getFileName().indexOf(".");
            prefix = options.getFileName().substring(0, index); // Used for setting file name
            fileSuffix = options.getFileName().substring(index);
        }

        try {
            String name;

            if ( ! ((FileInfoDicom) (myFileInfo)).isMultiFrame()) {

                for (i = options.getBeginSlice(); i <= options.getEndSlice(); i++) {
                    progressBar.updateValue(Math.round((float) i / (options.getEndSlice()) * 100), false);
                    myFileInfo = (FileInfoDicom) image.getFileInfo(i);
                    myFileInfo.setFileDirectory(fileDir); // need to update in case it changed

                    String s = "" + (i + 1);

                    if (options.isInstanceNumberRecalculated()) {
                        myFileInfo.getTagTable().setValue("0020,0013", s, s.length());
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
                    dicomFile.writeImage(image, i * sliceSize, (i * sliceSize) + sliceSize, i);
                }
            } else { // its a multi frame image to be saved!!!

                // progressBar.updateValue( Math.round((float)i/(endSlice) * 100));
                dicomFile = new FileDicom(fileName, fileDir); // was (UI, fileDir, fileDir). think this fixes...

                // String s=""+(i+1);
                // myFileInfo = (FileInfoDicom)image.getFileInfo(i);
                // if (saveAs) myFileInfo.updateValue("0020,0013", s, s.length());
                dicomFile.writeMultiFrameImage(image, options.getBeginSlice(), options.getEndSlice());
            }
        } catch (IOException error) {
            image.setFileInfo(originalFileInfos);

            error.printStackTrace();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (OutOfMemoryError error) {
            image.setFileInfo(originalFileInfos);

            error.printStackTrace();

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

        image.setFileInfo(originalFileInfos);
        if (dicomFile != null) {
            dicomFile.finalize();
            dicomFile = null;
        }
        return true;
    }

    /**
     * Writes a Fits file to store the image.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeFits(ModelImage image, FileWriteOptions options) {
        FileFits fitsFile;

        try { // Construct a new file object
            fitsFile = new FileFits(options.getFileName(), options.getFileDirectory());
            createProgressBar(fitsFile, options.getFileName(), FILE_WRITE);
            fitsFile.writeImage(image, options);
            fitsFile.finalize();
            fitsFile = null;
        } catch (IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (OutOfMemoryError error) {

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
    private boolean writeICS(ModelImage image, FileWriteOptions options) {
        FileICS ICSFile;

        try { // Construct a new file object

            ICSFile = new FileICS(options.getFileName(), options.getFileDirectory());
            createProgressBar(ICSFile, options.getFileName(), FILE_WRITE);
            ICSFile.writeImage(image, options);
        } catch (IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (OutOfMemoryError error) {

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
    private boolean writeInterfile(ModelImage image, FileWriteOptions options) {
        FileInterfile interfileFile;

        try { // Construct a new file object
            interfileFile = new FileInterfile(options.getFileName(), options.getFileDirectory());
            createProgressBar(interfileFile, options.getFileName(), FILE_WRITE);
            interfileFile.writeImage(image, options);
        } catch (IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (OutOfMemoryError error) {

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
    private boolean writeJimi(ModelImage image, FileWriteOptions options) {
        int index = options.getFileName().indexOf(".");
        String prefix = options.getFileName().substring(0, index); // Used for setting file name
        String fileSuffix = options.getFileName().substring(index);
        int slice = ((ViewJFrameImage) (image.getImageFrameVector().firstElement())).getComponentImage().getSlice();
        String name;

        int beginSlice = options.getBeginSlice();
        int endSlice = options.getEndSlice();

        for (int i = beginSlice; i <= endSlice; i++) {

            ((ViewJFrameImage) (image.getImageFrameVector().firstElement())).getComponentImage().show(0, i, true);

            Image im = ((ViewJFrameImage) (image.getImageFrameVector().firstElement())).getComponentImage().getImage();

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
            } catch (JimiException jimiException) {
                Preferences.debug("JIMI write error: " + jimiException + "\n", Preferences.DEBUG_FILEIO);

                jimiException.printStackTrace();

                return false;
            }
        }

        ((ViewJFrameImage) (image.getImageFrameVector().firstElement())).getComponentImage().show(0, slice, null, null,
                true, -1);

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
    private boolean writeMGH(ModelImage image, FileWriteOptions options) {
        FileMGH mghFile;

        try { // Construct a new file object
            mghFile = new FileMGH(options.getFileName(), options.getFileDirectory());
            createProgressBar(mghFile, options.getFileName(), FILE_WRITE);
            mghFile.writeImage(image, options);
        } catch (IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (OutOfMemoryError error) {

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
    private boolean writeMinc(ModelImage image, FileWriteOptions options) {
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

                JDialogSaveMinc dialog = new JDialogSaveMinc(UI.getMainFrame(), fileInfo, options);

                dialog.setVisible(true);

                if (dialog.isCancelled()) {
                    return false;
                }

                options = dialog.getOptions();
            }

            mincFile = new FileMinc(options.getFileName(), options.getFileDirectory());
            createProgressBar(mincFile, options.getFileName(), FILE_READ);
            mincFile.writeImage(image, options);

            return true;

        } catch (IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug(error.getMessage() + "\n", Preferences.DEBUG_FILEIO);
            }

            error.printStackTrace();

            return false;
        } catch (OutOfMemoryError error) {

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
    private boolean writeMincHDF(ModelImage image, FileWriteOptions options) {
        FileMincHDF mincFile;

        if (image.getNDims() != 3) {
            MipavUtil.displayError("FileIO: MINC writer only writes 3D images.");

            return false;
        }

        try { // Construct a new file object

            mincFile = new FileMincHDF(options.getFileName(), options.getFileDirectory());
            createProgressBar(mincFile, options.getFileName(), FILE_READ);

            if ( ! (image.getFileInfo()[0] instanceof FileInfoMincHDF)) {
                JDialogSaveMinc dialog = new JDialogSaveMinc(UI.getMainFrame(), image.getFileInfo()[0], options);
                dialog.setVisible(true);

                if (dialog.isCancelled()) {
                    return false;
                }

                options = dialog.getOptions();
            }

            if (image.getFileInfo()[0] instanceof FileInfoDicom) {

                if ( !dataConversion( ((FileInfoDicom) image.getFileInfo()[0]), mincFile.getFileInfo())) {
                    // do nothing
                }
            }

            mincFile.writeImage(image, options);

            return true;

        } catch (Exception error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug(error.getMessage() + "\n", Preferences.DEBUG_FILEIO);
            }

            error.printStackTrace();

            return false;
        } catch (OutOfMemoryError error) {

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
    private boolean writeMRC(ModelImage image, FileWriteOptions options) {
        FileMRC mrcFile;

        try { // Construct a new file object
            mrcFile = new FileMRC(options.getFileName(), options.getFileDirectory());
            createProgressBar(mrcFile, options.getFileName(), FILE_WRITE);
            mrcFile.writeImage(image, options);
        } catch (IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

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
    private boolean writeNIFTI(ModelImage image, FileWriteOptions options) {
        FileNIFTI NIFTIFile;

        try { // Construct a new file object
            NIFTIFile = new FileNIFTI(options.getFileName(), options.getFileDirectory());
            createProgressBar(NIFTIFile, options.getFileName(), FILE_WRITE);
            NIFTIFile.writeImage(image, options);
        } catch (IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (OutOfMemoryError error) {

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
     * Writes an OSM file to store the image.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeOSM(ModelImage image, FileWriteOptions options) {
        FileOSM osmFile;

        try { // Construct a new file object
            osmFile = new FileOSM(options.getFileName(), options.getFileDirectory());
            createProgressBar(osmFile, options.getFileName(), FILE_READ);
            osmFile.writeImage(image, options);
        } catch (IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

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
    private boolean writeRaw(ModelImage image, FileWriteOptions options) {
        FileRaw rawFile;
        FileInfoImageXML fileInfo;

        try { // Construct new file info and file objects
            fileInfo = new FileInfoImageXML(options.getFileName(), options.getFileDirectory(), FileUtility.RAW);
            rawFile = new FileRaw(options.getFileName(), options.getFileDirectory(), fileInfo, FileBase.READ_WRITE);
            createProgressBar(rawFile, options.getFileName(), FILE_WRITE);
            rawFile.writeImage(image, options);
        } catch (IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (OutOfMemoryError error) {

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
    private boolean writeSPM(ModelImage image, FileWriteOptions options) {
        FileSPM spmFile;

        try { // Construct a new file object
            spmFile = new FileSPM(options.getFileName(), options.getFileDirectory());
            createProgressBar(spmFile, options.getFileName(), FILE_WRITE);
            spmFile.writeImage(image, options);
        } catch (IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (OutOfMemoryError error) {

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
    private boolean writeTiff(ModelImage image, FileWriteOptions options) {
        FileTiff imageFile;
        int[] extents;

        try { // Construct a new file object
            imageFile = new FileTiff(options.getFileName(), options.getFileDirectory());
            createProgressBar(imageFile, options.getFileName(), FILE_WRITE);

            if (LUT == null) {
                extents = new int[2];
                extents[0] = 4;
                extents[1] = 256;
                ((FileTiff) imageFile).writeImage(image, new ModelLUT(1, 256, extents), options);
            } else {
                ((FileTiff) imageFile).writeImage(image, LUT, options);
            }
        } catch (IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

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
    private boolean writeXML(ModelImage image, FileWriteOptions options) {
        FileImageXML xmlFile;

        try {
            xmlFile = new FileImageXML(options.getFileName(), options.getFileDirectory());
            createProgressBar(xmlFile, options.getFileName(), FILE_WRITE);

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
        } catch (IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (OutOfMemoryError error) {

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

    private boolean writePARREC(ModelImage image, FileWriteOptions options) {

        try { // Construct new file info and file objects

            FileInfoBase fileBase = image.getFileInfo()[0];
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
        } catch (IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (OutOfMemoryError error) {

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
    private boolean writeNRRD(ModelImage image, FileWriteOptions options) {
        try {
            FileNRRD fileNRRD = new FileNRRD(options.getFileName(), options.getFileDirectory());
            createProgressBar(fileNRRD, options.getFileName(), FileIO.FILE_WRITE);
            fileNRRD.writeImage(image, options);
        } catch (IOException error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        } catch (OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            error.printStackTrace();

            return false;
        }

        return true;
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
        public OrientStatus(int ind, float loc) {
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
        public boolean equals(Object o) {

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
