package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.file.FileTypeTable;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;

import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogSaveMinc;

import java.io.File;


/**
 * A script action which writes out an image to disk.
 */
public abstract class ActionSaveBase extends ActionImageProcessorBase {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Label for the file name prefix parameter. */
    public static final String SAVE_PREFIX = "file_name_prefix";

    /** Label for the file name suffix parameter. */
    public static final String SAVE_SUFFIX = "file_name_suffix";

    /** Label for the file name parameter (overrides the file_type parameter). */
    public static final String SAVE_FILE_NAME = "file_name";

    /** Label for the file type (extension, generally) parameter. */
    public static final String SAVE_FILE_TYPE = "file_type";

    /** Label for first slice to save from an image parameter. */
    public static final String START_SLICE = "start_slice";

    /** Label for last slice to save from an image parameter. */
    public static final String END_SLICE = "end_slice";

    /** Label for the tiff set write pack bit parameter. */
    public static final String TIFF_SET_WRITE_PACK_BIT = "tiff_do_write_pack_bet";

    /** Label for the tiff initial image number parameter. */
    public static final String TIFF_START_NUMBER = "tiff_start_number";

    /** Label for the parameter indicating the number of digits to use in tiff image file naming. */
    public static final String TIFF_DIGIT_NUMBER = "tiff_digit_number";

    /** Label for the parameter indicating the first time slice to save. */
    public static final String START_TIME = "start_time";

    /** Label for the parameter indicating the last time slice to save. */
    public static final String END_TIME = "end_time";

    /**
     * Label for the parameter indicating which time slice to save when saving 4D images in a format which only supports
     * 3D image writing.
     */
    public static final String TIME_SLICE = "time_slice";

    /** Label for the avi compression parameter. */
    public static final String AVI_COMPRESSION = "avi_compression";
    
    /** Label for the parameter indicating whether .nii or .hdr/.img is used in nifti file writes */
    public static final String NIFTI_EXTENSION = "nifti_extension";
    
    

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /**
     * The file saving options used to save the image, which should be recording in the script. The action saving must
     * be done elsewhere.
     */
    protected FileWriteOptions recordingOptions;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for the dynamic instantiation and execution of the script action.
     */
    public ActionSaveBase() {
        super();
    }

    /**
     * Constructor used to record the script action line.
     * 
     * @param input The image which was saved.
     * @param options The options used during the image save.
     */
    public ActionSaveBase(ModelImage input, FileWriteOptions options) {
        super(input);
        recordingOptions = options;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Changes the set of options used to save the image.
     * 
     * @param options The options used to save the image.
     */
    public void setSaveOptions(FileWriteOptions options) {
        recordingOptions = options;
    }

    /**
     * Create a new set of image saving options from a save action's parameters.
     * 
     * @param parameters The save script action parameters.
     * @param image The image to create the save options for.
     * @param isSaveAsAction Whether this save action is a SaveAs or just a Save.
     * 
     * @return The options which should be used to save the given image.
     */
    protected static final FileWriteOptions getSaveImageOptions(ParameterTable parameters, ModelImage image,
            boolean isSaveAsAction) {
        String savePrefix = "";
        String saveFileDir = "";
        String saveFileName = null;

        if (parameters.containsParameter(SAVE_PREFIX)) {
            savePrefix = parameters.getString(SAVE_PREFIX);
        }

        String saveSuffix = "";

        if (parameters.containsParameter(SAVE_SUFFIX)) {
            saveSuffix = parameters.getString(SAVE_SUFFIX);
        }
        
        if(ViewUserInterface.getReference().isProvidedOutputDir()) {
        	saveFileDir = ViewUserInterface.getReference().getDefaultDirectory();
        }else {
	        saveFileDir = image.getImageDirectory();
	        
	        if ( (saveFileDir == null) || saveFileDir.equals("")) {
	            saveFileDir = ViewUserInterface.getReference().getDefaultDirectory();
	        }
        }
        
        

        saveFileDir += File.separator;

        // prefer the file name over the file type if it is set
        // TODO: maybe make the file type change the type of the file name?
        if (isSaveAsAction && parameters.containsParameter(SAVE_FILE_NAME)) {
            saveFileName = FileUtility.getFileName(parameters.getString(SAVE_FILE_NAME));

            saveFileDir = FileUtility.getFileDirectory(parameters.getString(SAVE_FILE_NAME));

            if (saveFileDir == null) {
            	if(ViewUserInterface.getReference().isProvidedOutputDir()) {
                	saveFileDir = ViewUserInterface.getReference().getDefaultDirectory();
                }else {
                	saveFileDir = image.getImageDirectory();

                	if (saveFileDir == null) {
                		saveFileDir = ViewUserInterface.getReference().getDefaultDirectory();
                	}
                }
            }

            String extension = FileUtility.getExtension(saveFileName);

            saveFileName = savePrefix + FileUtility.stripExtension(saveFileName) + saveSuffix + extension;

            int collisionAvoidanceIndex = 1;
            File testImageFile = new File(saveFileDir, saveFileName);

            while (testImageFile.exists()) {
                Preferences.debug(testImageFile.getAbsolutePath() + " already exists.\n", Preferences.DEBUG_SCRIPTING);

                String newSaveFileName = FileUtility.stripExtension(saveFileName) + "_" + collisionAvoidanceIndex
                        + extension;
                testImageFile = null;
                testImageFile = new File(saveFileDir, newSaveFileName);

                collisionAvoidanceIndex++;
            }
        } else if (isSaveAsAction && parameters.containsParameter(SAVE_FILE_TYPE)) {
            String extension = parameters.getString(SAVE_FILE_TYPE);
            saveFileName = savePrefix + image.getImageName() + saveSuffix + extension;

            int collisionAvoidanceIndex = 1;
            File testImageFile = new File(saveFileDir, saveFileName);

            while (testImageFile.exists()) {
                Preferences.debug(testImageFile.getAbsolutePath() + " already exists.\n", Preferences.DEBUG_SCRIPTING);

                saveFileName = savePrefix + image.getImageName() + saveSuffix + "_" + collisionAvoidanceIndex
                        + extension;
                testImageFile = null;
                testImageFile = new File(saveFileDir, saveFileName);

                collisionAvoidanceIndex++;
            }
        } else {

            // not a save as action
            String extension = FileUtility.getExtension(image.getImageFileName());
            saveFileName = savePrefix + image.getImageName() + saveSuffix + extension;

            int collisionAvoidanceIndex = 1;
            File testImageFile = new File(saveFileDir, saveFileName);

            while (testImageFile.exists()) {
                Preferences.debug(testImageFile.getAbsolutePath() + " already exists.\n", Preferences.DEBUG_SCRIPTING);

                saveFileName = savePrefix + image.getImageName() + saveSuffix + "_" + collisionAvoidanceIndex
                        + extension;
                testImageFile = null;
                testImageFile = new File(saveFileDir, saveFileName);

                collisionAvoidanceIndex++;
            }
        }

        FileWriteOptions opts = new FileWriteOptions(saveFileName, saveFileDir, isSaveAsAction);

        opts.setRunningInSeparateThread(false);
        opts.setOptionsSet(true);
        opts.setIsScript(true);

        int fileType = FileUtility.getFileType(opts.getFileName(), opts.getFileDirectory(), false);
        Preferences.debug("File type is: " + fileType + "\n", Preferences.DEBUG_SCRIPTING);
        opts.setFileType(fileType);

        // tiff only parameter (and may be optional even for tiffs)
        opts.setPackBitEnabled( (fileType == FileUtility.TIFF)
                && ( (image.getFileInfo(0).getDataType() == ModelStorageBase.BYTE) || (image.getFileInfo(0)
                        .getDataType() == ModelStorageBase.UBYTE)));

        if (parameters.containsParameter(TIFF_SET_WRITE_PACK_BIT)) {
            boolean isWritePackBitSet = parameters.getBoolean(TIFF_SET_WRITE_PACK_BIT);
            opts.setWritePackBit(isWritePackBitSet);
        }

        // set defaults and then maybe override them from the params later
        if (image.getNDims() == 3) {
            opts.setBeginSlice(0);
            opts.setEndSlice(image.getExtents()[2] - 1);
            opts.setAxisOrientation(image.getAxisOrientation());

            if (fileType == FileUtility.TIFF) {
                opts.setWritePackBit(false);
                opts.setMultiFile(false);
            } else if (fileType == FileUtility.MINC) {
                JDialogSaveMinc minc = new JDialogSaveMinc(ViewUserInterface.getReference().getMainFrame(), image
                        .getFileInfo(0), opts);
                opts = minc.setOptionsDefault();
            }
        } else if (image.getNDims() == 4) {
            opts.setBeginSlice(0);
            opts.setEndSlice(image.getExtents()[2] - 1);
            opts.setBeginTime(0);
            opts.setEndTime(image.getExtents()[3] - 1);
            opts.setTimeSlice(0);

            if (fileType == FileUtility.TIFF) {
                opts.setWritePackBit(false);
                opts.setMultiFile(false);
            } else if (fileType == FileUtility.MINC) {
                JDialogSaveMinc minc = new JDialogSaveMinc(ViewUserInterface.getReference().getMainFrame(), image
                        .getFileInfo(0), opts);
                opts = minc.setOptionsDefault();
            }
        }

        // allow parameters to override the option defaults
        if (fileType == FileUtility.TIFF) {

            try {
                int startNumber = parameters.getInt(TIFF_START_NUMBER);
                int digitNumber = parameters.getInt(TIFF_DIGIT_NUMBER);

                opts.setStartNumber(startNumber);
                opts.setDigitNumber(digitNumber);
                opts.setMultiFile(true);
            } catch (ParameterException pe) {
                Preferences.debug(pe + ".  It is an optional parameter.  Using defaults.\n",
                        Preferences.DEBUG_SCRIPTING);
            }
        } else if (fileType == FileUtility.AVI) {

            try {
                int aviCompression = parameters.getInt(AVI_COMPRESSION);
                opts.setAVICompression(aviCompression);
            } catch (ParameterException pe) {

                // the above params are optional
                Preferences.debug(pe + ".  It is an optional parameter.  Using defaults.\n",
                        Preferences.DEBUG_SCRIPTING);
            }
        } else if (fileType == FileUtility.NIFTI) {
        	try {
        		String niftiExtension = parameters.getString(NIFTI_EXTENSION);
        		opts.setNIFTIExtension(niftiExtension);
        	} catch (ParameterException pe) {

                // the above params are optional
                Preferences.debug(pe + ".  It is an optional parameter.  Using defaults.\n",
                        Preferences.DEBUG_SCRIPTING);
            }
        }

        if (image.getNDims() >= 3) {

            try {
                int startSlice = parameters.getInt(START_SLICE);
                int endSlice = parameters.getInt(END_SLICE);

                opts.setBeginSlice(startSlice);
                opts.setEndSlice(endSlice);
            } catch (ParameterException pe) {

                // the above params are optional
                Preferences.debug(pe + ".  It is an optional parameter.  Using defaults.\n",
                        Preferences.DEBUG_SCRIPTING);
            }
        }

        if (image.getNDims() == 4) {

            if (parameters.containsParameter(START_TIME) && parameters.containsParameter(END_TIME)) {
                int startTime = parameters.getInt(START_TIME);
                int endTime = parameters.getInt(END_TIME);
                opts.setBeginTime(startTime);
                opts.setEndTime(endTime);
            } else if (parameters.containsParameter(TIME_SLICE)) {
                int timeSlice = parameters.getInt(TIME_SLICE);
                opts.setTimeSlice(timeSlice);
            }
        }

        return opts;
    }

    /**
     * Add the save options needed to record the saving of an image to a parameter table.
     * 
     * @param parameters The parameter table to add the save parameters to.
     * @param options The save options used in the image save.
     * @param extents The extents of the image which was saved.
     * 
     * @throws ParserException If there is a problem creating the parameters.
     */
    protected void addSaveOptionsToParameters(ParameterTable parameters, FileWriteOptions options, int[] extents)
            throws ParserException {
        int nDims = extents.length;

        // if not a save as, always save the same type of file
        if (options.isSaveAs()) {

        	if(options.isGzip()) {
        		parameters.put(ParameterFactory.newString(SAVE_FILE_TYPE, FileTypeTable.getFileTypeInfo(options.getFileType()).getDefaultExtension() + ".gz"));
        	}else if(options.isZip()) {
        		parameters.put(ParameterFactory.newString(SAVE_FILE_TYPE, FileTypeTable.getFileTypeInfo(options.getFileType()).getDefaultExtension() + ".zip"));
        	}else if(options.isBz2zip()) {
        		parameters.put(ParameterFactory.newString(SAVE_FILE_TYPE, FileTypeTable.getFileTypeInfo(options.getFileType()).getDefaultExtension() + ".bz2"));
        	}else {
        		parameters.put(ParameterFactory.newString(SAVE_FILE_TYPE, FileTypeTable.getFileTypeInfo(options.getFileType()).getDefaultExtension()));

        	}
        	
        	
            
        	
        	
        	
            // do not record the file name used if recording a script line
            if ( !isScript()) {
                // added this here specifically for ProvenanceRecorder so that the save-as filename will be shown
                parameters.put(ParameterFactory.newString(SAVE_FILE_NAME, options.getFileName()));
            }
        }

        if ( (options.getFileType() == FileUtility.TIFF) && options.isPackBitEnabled()) {
            parameters.put(ParameterFactory.newBoolean(TIFF_SET_WRITE_PACK_BIT, options.isWritePackBit()));
        }

        if ( (options.getFileType() == FileUtility.AVI)) {
            parameters.put(ParameterFactory.newInt(AVI_COMPRESSION, options.getAVICompression()));
        }
        
        if (options.getFileType() == FileUtility.NIFTI) {
        	String ext = FileUtility.getExtension(options.getFileName());
        	if ((ext.equalsIgnoreCase(".hdr")) || (ext.equalsIgnoreCase(".img"))) {
        	    parameters.put(ParameterFactory.newString(NIFTI_EXTENSION, ext));
        	}
        }

        if (nDims == 3) {

            if ( (options.getBeginSlice() != 0) || (options.getEndSlice() != (extents[2] - 1))) {
                parameters.put(ParameterFactory.newInt(START_SLICE, options.getBeginSlice()));
                parameters.put(ParameterFactory.newInt(END_SLICE, options.getEndSlice()));
            }

            if ( (options.getFileType() == FileUtility.TIFF) && options.isMultiFile()) {
                parameters.put(ParameterFactory.newInt(TIFF_START_NUMBER, options.getStartNumber()));
                parameters.put(ParameterFactory.newInt(TIFF_DIGIT_NUMBER, options.getDigitNumber()));
            }
        } else if (nDims == 4) {

            if ( (options.getBeginSlice() != 0) || (options.getEndSlice() != (extents[2] - 1))) {
                parameters.put(ParameterFactory.newInt(START_SLICE, options.getBeginSlice()));
                parameters.put(ParameterFactory.newInt(END_SLICE, options.getEndSlice()));
            }

            // we can read 4d minc and tiff, but can't write out again as 4d (so just save a specific time point)
            if ( (options.getFileType() == FileUtility.TIFF) || (options.getFileType() == FileUtility.MINC)) {
                parameters.put(ParameterFactory.newInt(TIME_SLICE, options.getTimeSlice()));

                if (options.isMultiFile()) {
                    parameters.put(ParameterFactory.newInt(TIFF_START_NUMBER, options.getStartNumber()));
                    parameters.put(ParameterFactory.newInt(TIFF_DIGIT_NUMBER, options.getDigitNumber()));
                }
            } else {

                if ( (options.getBeginTime() != 0) || (options.getEndTime() != (extents[3] - 1))) {
                    parameters.put(ParameterFactory.newInt(START_TIME, options.getBeginTime()));
                    parameters.put(ParameterFactory.newInt(END_TIME, options.getEndTime()));
                }
            }
        }
    }
}
