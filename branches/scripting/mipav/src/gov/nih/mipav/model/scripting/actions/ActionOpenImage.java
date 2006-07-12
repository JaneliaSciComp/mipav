package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.*;


/**
 * A script action which opens an image for use in MIPAV.
 */
public class ActionOpenImage extends ActionImageProcessorBase {
    
    /** Label of the parameter indicating whether we should try to open multi-file-capable image formats as multi-file images. */
    private static final String FILE_ALLOW_MULTIFILE = "allow_multifile_open";
    
    /** Label of the parameter indicating the data type of the raw image. */
    private static final String FILE_RAW_DATA_TYPE = "raw_data_type";
    
    /** Label of the parameter indicating whether the raw image is big endian. */
    private static final String FILE_RAW_ENDIANESS = "is_raw_big_endian";
    
    /** Label of the parameter indicating the file offset. */
    private static final String FILE_RAW_OFFSET = "raw_offset";
    
    /** Label of the parameter indicating the extents of the raw image. */
    private static final String FILE_RAW_EXTENTS = "raw_extents";
    
    /** Label of the parameter indicating the resolutions of the raw image. */
    private static final String FILE_RAW_RESOLUTIONS = "raw_resolutions";
    
    /** Label of the parameter indicating the units of measure of the raw image. */
    private static final String FILE_RAW_UNITS = "raw_units";
    
    /**
     * Whether the image which was opened was opened with the multi-file option checked.
     */
    private boolean recordingAllowMultiFile;
    
    /**
     * Constructor for the dynamic instantiation and execution of the OpenImage script action.
     */
    public ActionOpenImage() {
        super();
    }
    
    /**
     * Constructor used to record the OpenImage script action line.
     * @param  input           The image which was opened.
     * @param  allowMultiFile  Whether to try to open files as multi-file images.
     */
    public ActionOpenImage(ModelImage input, boolean allowMultiFile) {
        super(input);
        recordingAllowMultiFile = allowMultiFile;
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
    public void insertScriptLine() {
        ParameterTable parameters = new ParameterTable();
        try {
            parameters.put(createInputImageParameter());
            parameters.put(ParameterFactory.newBoolean(FILE_ALLOW_MULTIFILE, recordingAllowMultiFile));
            
            int fileFormat = recordingInputImage.getFileInfo(0).getFileFormat();
            if (fileFormat == FileBase.RAW || fileFormat == FileBase.RAW_MULTIFILE) {
                ActionOpenImage.addRawOptionsToParameters(parameters, recordingInputImage.getFileInfo(0));
            }
        } catch (ParserException pe) {
            MipavUtil.displayError("Error encountered creating parameters while recording " + getActionName() + " script action:\n" + pe);
            return;
        }
        
        ScriptRecorder.getReference().addLine(getActionName(), parameters);
    }

    /**
     * {@inheritDoc}
     */
    public void scriptRun(ParameterTable parameters) {
        ViewOpenFileUI fileUI = new ViewOpenFileUI(false);
        FileIO io = new FileIO();
        
        String imageName;
        
        // TODO: how do we get the image filename/dir that we should open?
        String fileDir = null;
        String fileName = null;
        boolean tryOpenAsMultiFile = parameters.getBoolean(FILE_ALLOW_MULTIFILE);
        
        int fileType = io.getFileType(fileName, fileDir);
        if (fileType == FileBase.RAW || fileType == FileBase.RAW_MULTIFILE) {
            imageName = fileUI.open(fileDir + fileName, tryOpenAsMultiFile, getRawFileInfo(parameters, tryOpenAsMultiFile));
        } else {
            imageName = fileUI.open(fileDir + fileName, tryOpenAsMultiFile, null);
        }

        // if the SaveAllOnSave preference flag is set, then
        // load all the files associated with this image (VOIs, LUTs, etc.)
        if (Preferences.is(Preferences.PREF_SAVE_ALL_ON_SAVE)) {
            ModelImage img = ViewUserInterface.getReference().getRegisteredImageByName(imageName);

            // get frame for image
            ViewJFrameImage imgFrame = img.getParentFrame();

            // load any luts
            imgFrame.loadLUT(true, true);

            // load any vois
            imgFrame.loadAllVOIs(true);
        }

        // TODO: i don't believe that this is the correct way for the image to be added to the variable table
        ScriptRunner.getReference().getImageTable().storeImageName(imageName);
    }
    
    /**
     * Adds parameters to a table, describing a raw image.
     * 
     * @param  parameters  The table to add the parameters to.  
     * @param  fileInfo    The file info to get the raw image description from.
     * 
     * @throws ParserException  If there is a problem encountered adding the parameters.
     */
    public static final void addRawOptionsToParameters(ParameterTable parameters, FileInfoBase fileInfo) throws ParserException {
        parameters.put(ParameterFactory.newInt(FILE_RAW_DATA_TYPE, fileInfo.getDataType()));
        parameters.put(ParameterFactory.newBoolean(FILE_RAW_ENDIANESS, fileInfo.getEndianess()));
        parameters.put(ParameterFactory.newInt(FILE_RAW_OFFSET, fileInfo.getOffset()));
        parameters.put(ParameterFactory.newParameter(FILE_RAW_EXTENTS, fileInfo.getExtents()));
        parameters.put(ParameterFactory.newParameter(FILE_RAW_RESOLUTIONS, fileInfo.getResolutions()));
        parameters.put(ParameterFactory.newParameter(FILE_RAW_UNITS, fileInfo.getUnitsOfMeasure()));
    }
    
    /**
     * Reads in the necessary file info from a set of parameters to read a RAW file.
     *
     * @param   parameters          The action parameters.
     * @param   tryOpenAsMultiFile  Whether the file we are opening may be a multi-file image.
     *                              TODO: i don't believe that this guarantees that the image we opened was a multi-file raw, does it?
     *
     * @return  The file info necessary to read in the RAW file.
     */
    public static final FileInfoBase getRawFileInfo(ParameterTable parameters, boolean tryOpenAsMultiFile) {
        FileInfoBase fileInfo = null;

        // TODO: we need to figure out a good way to get the list of image files/dirs from the script run dialog and command line to this action..
        /*if (tryOpenAsMultiFile) {
            fileInfo = new FileInfoImageXML((String) fileNames.elementAt(currFileIndex),
                                            (String) fileDirs.elementAt(currFileIndex), FileBase.RAW_MULTIFILE);
        } else {
            fileInfo = new FileInfoImageXML((String) fileNames.elementAt(currFileIndex),
                                            (String) fileDirs.elementAt(currFileIndex), FileBase.RAW);
        }*/

        fileInfo.setDataType(parameters.getInt(FILE_RAW_DATA_TYPE));
        fileInfo.setEndianess(parameters.getBoolean(FILE_RAW_ENDIANESS));
        fileInfo.setOffset(parameters.getInt(FILE_RAW_OFFSET));

        int[] extents = parameters.getList(FILE_RAW_EXTENTS).getAsIntArray();
        float[] res = parameters.getList(FILE_RAW_RESOLUTIONS).getAsFloatArray();
        int[] measure = parameters.getList(FILE_RAW_UNITS).getAsIntArray();

        fileInfo.setExtents(extents);
        fileInfo.setUnitsOfMeasure(measure);
        fileInfo.setResolutions(res);

        return fileInfo;
    }
}
