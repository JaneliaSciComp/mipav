package gov.nih.mipav.model.scripting.actions;


import java.io.File;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.AlgorithmParameters;
import gov.nih.mipav.view.dialogs.JDialogSaveMinc;


/**
 * A script action which writes out an image to disk.
 */
public abstract class ActionSaveBase implements ScriptableActionInterface {

    /**
     * The label to use for the input image parameter.
     */
    protected static final String INPUT_IMAGE_LABEL = AlgorithmParameters.getInputImageLabel(1);
    
    /**
     * Label for the file name prefix parameter.
     */
    protected static final String SAVE_PREFIX = "file_name_prefix";
    
    /**
     * Label for the file name suffix parameter.
     */
    protected static final String SAVE_SUFFIX = "file_name_suffix";
    
    /**
     * Label for the file name parameter (overrides the file_type parameter).
     */
    protected static final String SAVE_FILE_NAME = "file_name";
    
    /**
     * Label for the file type (extension, generally) parameter.
     */
    protected static final String SAVE_FILE_TYPE = "file_type";
    
    /**
     * Label for first slice to save from an image parameter.
     */
    protected static final String START_SLICE = "start_slice";
    
    /**
     * Label for last slice to save from an image parameter.
     */
    protected static final String END_SLICE = "end_slice";
    
    /**
     * Label for the tiff set write pack bit parameter.
     */
    protected static final String TIFF_SET_WRITE_PACK_BIT = "tiff_do_write_pack_bet";
    
    /**
     * Label for the tiff initial image number parameter.
     */
    protected static final String TIFF_START_NUMBER = "tiff_start_number";
    
    /**
     * Label for the parameter indicating the number of digits to use in tiff image file naming.
     */
    protected static final String TIFF_DIGIT_NUMBER = "tiff_digit_number";
    
    /**
     * Label for the parameter indicating the first time slice to save.
     */
    protected static final String START_TIME = "start_time";
    
    /**
     * Label for the parameter indicating the last time slice to save.
     */
    protected static final String END_TIME = "end_time";
    
    /**
     * Label for the parameter indicating which time slice to save when saving 4D images in a format which only supports 3D image writing.
     */
    protected static final String TIME_SLICE = "time_slice";
    
    /**
     * Label for the avi compression parameter.
     */
    protected static final String AVI_COMPRESSION = "avi_compression";
    
    /**
     * The image whose saving should be recorded in the script.  The actual saving must be done elsewhere.
     */
    protected ModelImage recordingInputImage;
    
    /**
     * The file saving options used to save the image, which should be recording in the script.  The action saving must be done elsewhere.
     */
    protected FileWriteOptions recordingOptions;
    
    /**
     * Constructor for the dynamic instantiation and execution of the script action.
     */
    public ActionSaveBase() {}
    
    /**
     * Constructor used to record the script action line.
     * @param input    The image which was saved.
     * @param options  The options used during the image save.
     */
    public ActionSaveBase(ModelImage input, FileWriteOptions options) {
        recordingInputImage = input;
        recordingOptions = options;
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
    public abstract void insertScriptLine();

    /**
     * {@inheritDoc}
     */
    public abstract void scriptRun(ParameterTable parameters);
    
    /**
     * Changes the image which was saved.
     * @param inputImage  The image which was saved.
     */
    public void setInputImage(ModelImage inputImage) {
        recordingInputImage = inputImage;
    }
    
    /**
     * Changes the set of options used to save the image.
     * @param  options  The options used to save the image.
     */
    public void setSaveOptions(FileWriteOptions options) {
        recordingOptions = options;
    }
    
    /**
     * Create a new set of image saving options from a save action's parameters.
     * @param parameters      The save script action parameters.
     * @param image           The image to create the save options for.
     * @param isSaveAsAction  Whether this save action is a SaveAs or just a Save.
     * @return  The options which should be used to save the given image.
     */
    protected static final FileWriteOptions getSaveImageOptions(ParameterTable parameters, ModelImage image, boolean isSaveAsAction) {
        String savePrefix = "";
        if (parameters.containsParameter(SAVE_PREFIX)) {
            savePrefix = parameters.getString(SAVE_PREFIX);
        }
        
        String saveSuffix = "";
        if (parameters.containsParameter(SAVE_SUFFIX)) {
            saveSuffix = parameters.getString(SAVE_SUFFIX);
        }
        
        FileIO fileIO = new FileIO();
        
        String saveFileName = null;
        String saveFileDir = image.getImageDirectory();
        if (saveFileDir == null || saveFileDir.equals("")) {
            saveFileDir = ViewUserInterface.getReference().getDefaultDirectory();
        }
        saveFileDir += File.separator;
        
        // prefer the file name over the file type if it is set
        // TODO: maybe make the file type change the type of the file name?
        if (parameters.containsParameter(SAVE_FILE_NAME)) {
            String extension = ActionSaveBase.getFileExtension(saveFileName);
            saveFileName = savePrefix + ActionSaveBase.stripExtension(saveFileName) + saveSuffix + extension;

            int collisionAvoidanceIndex = 1;
            File testImageFile = new File(saveFileDir, saveFileName);
            while (testImageFile.exists()) {
                Preferences.debug(testImageFile.getAbsolutePath() + " already exists.\n", Preferences.DEBUG_MINOR);
                
                String newSaveFileName = ActionSaveBase.stripExtension(saveFileName) + "_" + collisionAvoidanceIndex + extension;
                testImageFile = null;
                testImageFile = new File(saveFileDir, newSaveFileName);
                
                collisionAvoidanceIndex++;
            }
        } else if (parameters.containsParameter(SAVE_FILE_TYPE)) {
            String extension = parameters.getString(SAVE_FILE_TYPE);
            saveFileName = savePrefix + image.getImageName() + saveSuffix + extension;
            
            int collisionAvoidanceIndex = 1;
            File testImageFile = new File(saveFileDir, saveFileName);
            while (testImageFile.exists()) {
                Preferences.debug(testImageFile.getAbsolutePath() + " already exists.\n", Preferences.DEBUG_MINOR);
                
                saveFileName = savePrefix + image.getImageName() + saveSuffix + "_" + collisionAvoidanceIndex + extension;
                testImageFile = null;
                testImageFile = new File(saveFileDir, saveFileName);
                
                collisionAvoidanceIndex++;
            }
        }
        
        // TODO: is this correct?
        //if (runningInSeparateThread) {
        //} else {
        //    opts = new FileWriteOptions(fname, (String) fileDirs.elementAt(currFileIndex) + File.separator, saveAs);
        //}
        
        FileWriteOptions opts = new FileWriteOptions(saveFileName, saveFileDir, isSaveAsAction);

        opts.setRunningInSeparateThread(false);
        opts.setOptionsSet(true);
        opts.setIsScript(true);
        
        int fileType = fileIO.getFileType(opts.getFileName(), opts.getFileDirectory());
        Preferences.debug("File type is: " + fileType + "\n", Preferences.DEBUG_MINOR);
        opts.setFileType(fileType);
        
        // tiff only parameter (and may be optional even for tiffs)
        opts.setPackBitEnabled((fileType == FileBase.TIFF) &&
                                   ((image.getFileInfo(0).getDataType() == ModelStorageBase.BYTE) ||
                                        (image.getFileInfo(0).getDataType() == ModelStorageBase.UBYTE)));
        if (parameters.containsParameter(TIFF_SET_WRITE_PACK_BIT)) {
            boolean isWritePackBitSet = parameters.getBoolean(TIFF_SET_WRITE_PACK_BIT);
            opts.setWritePackBit(isWritePackBitSet);
        }
        
        // set defaults and then maybe override them from the params later
        if (image.getNDims() == 3) {
            opts.setBeginSlice(0);
            opts.setEndSlice(image.getExtents()[2] - 1);

            if (fileType == FileBase.TIFF) {
                opts.setWritePackBit(false);
                opts.setMultiFile(false);
            } else if (fileType == FileBase.MINC) {
                JDialogSaveMinc minc = new JDialogSaveMinc(ViewUserInterface.getReference().getMainFrame(), image.getFileInfo(0), opts);
                opts = minc.setOptionsDefault();
            }
        } else if (image.getNDims() == 4) {
            opts.setBeginSlice(0);
            opts.setEndSlice(image.getExtents()[2] - 1);
            opts.setBeginTime(0);
            opts.setEndTime(image.getExtents()[3] - 1);
            opts.setTimeSlice(0);

            if (fileType == FileBase.TIFF) {
                opts.setWritePackBit(false);
                opts.setMultiFile(false);
            } else if (fileType == FileBase.MINC) {
                JDialogSaveMinc minc = new JDialogSaveMinc(ViewUserInterface.getReference().getMainFrame(), image.getFileInfo(0), opts);
                opts = minc.setOptionsDefault();
            }
        }
        
        // allow parameters to override the option defaults
        if (fileType == FileBase.TIFF) {
            try {
                int startNumber = parameters.getInt(TIFF_START_NUMBER);
                int digitNumber = parameters.getInt(TIFF_DIGIT_NUMBER);
            
                opts.setStartNumber(startNumber);
                opts.setDigitNumber(digitNumber);
                opts.setMultiFile(true);
            } catch (ParameterException pe) {
                Preferences.debug(pe + ".  It is an optional parameter.  Using defaults.\n", Preferences.DEBUG_MINOR);
            }
        } else if (fileType == FileBase.AVI) {
            try {
                int aviCompression = parameters.getInt(AVI_COMPRESSION);
                opts.setAVICompression(aviCompression);
            } catch (ParameterException pe) {
                // the above params are optional
                Preferences.debug(pe + ".  It is an optional parameter.  Using defaults.\n", Preferences.DEBUG_MINOR);
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
                Preferences.debug(pe + ".  It is an optional parameter.  Using defaults.\n", Preferences.DEBUG_MINOR);
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
     * @param parameters  The parameter table to add the save parameters to.
     * @param options     The save options used in the image save.
     * @param extents     The extents of the image which was saved.
     * @throws ParserException  If there is a problem creating the parameters.
     */
    protected void addSaveOptionsToParameters(ParameterTable parameters, FileWriteOptions options, int[] extents) throws ParserException {
        int nDims = extents.length;

        if ((options.getFileType() == FileBase.TIFF) && options.isPackBitEnabled()) {
            parameters.put(ParameterFactory.newBoolean(TIFF_SET_WRITE_PACK_BIT, options.isWritePackBit()));
        }
        
        if ((options.getFileType() == FileBase.AVI)) {
            parameters.put(ParameterFactory.newInt(AVI_COMPRESSION, options.getAVICompression()));
        }

        if (nDims == 3) {
            parameters.put(ParameterFactory.newInt(START_SLICE, options.getBeginSlice()));
            parameters.put(ParameterFactory.newInt(END_SLICE, options.getEndSlice()));

            if ((options.getFileType() == FileBase.TIFF) && options.isMultiFile()) {
                parameters.put(ParameterFactory.newInt(TIFF_START_NUMBER, options.getStartNumber()));
                parameters.put(ParameterFactory.newInt(TIFF_DIGIT_NUMBER, options.getDigitNumber()));
            }
        } else if (nDims == 4) {
            parameters.put(ParameterFactory.newInt(START_SLICE, options.getBeginSlice()));
            parameters.put(ParameterFactory.newInt(END_SLICE, options.getEndSlice()));

            // we can read 4d minc and tiff, but can't write out again as 4d (so just save a specific time point)
            if ((options.getFileType() == FileBase.TIFF) || (options.getFileType() == FileBase.MINC)) {
                parameters.put(ParameterFactory.newInt(TIME_SLICE, options.getTimeSlice()));

                if (options.isMultiFile()) {
                    parameters.put(ParameterFactory.newInt(TIFF_START_NUMBER, options.getStartNumber()));
                    parameters.put(ParameterFactory.newInt(TIFF_DIGIT_NUMBER, options.getDigitNumber()));
                }
            } else {
                parameters.put(ParameterFactory.newInt(START_TIME, options.getBeginTime()));
                parameters.put(ParameterFactory.newInt(END_TIME, options.getEndTime()));                
            }
        }
    }
    
    /**
     * Helper method to strip the image name of the extension, so when we save we don't have double extensions (like
     * genormcor.img.tif).
     *
     * @param  fileName  Original name.
     *
     * @return  Name without extension, or original name if there was no extension.
     */
    protected static final String stripExtension(String fileName) {
        int index = fileName.lastIndexOf(".");

        if (index != -1) {
            return fileName.substring(0, index);
        } else {
            return fileName;
        }
    }
    
    /**
     * Returns the extension of a file name.
     * @param  fileName  A file name.
     * @return  The file name's extension (or an empty string if no extension).
     */
    protected static final String getFileExtension(String fileName) {
        int index = fileName.lastIndexOf(".");
        
        if (index != -1) {
            return fileName.substring(index);
        } else {
            // no extension
            return "";
        }
    }
}
