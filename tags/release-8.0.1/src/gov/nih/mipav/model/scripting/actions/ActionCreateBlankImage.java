package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewUserInterface;


/**
 * A script action which creates a new blank image with a set of characteristics.
 */
public class ActionCreateBlankImage extends ActionImageProcessorBase {
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
     * Constructor for the dynamic instantiation and execution of the CreateBlankImage script action.
     */
    public ActionCreateBlankImage() {
        super();
    }
    
    /**
     * Constructor used to record the CreateBlankImage script action line.
     * @param input  The blank image which was created.
     */
    public ActionCreateBlankImage(ModelImage input) {
        super(input);
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
    public void insertScriptLine() {
        ParameterTable parameters = new ParameterTable();
        try {
            addRawOptionsToParameters(parameters, recordingInputImage.getFileInfo(0));
        } catch (ParserException pe) {
            MipavUtil.displayError("Error encountered creating parameters while recording " + getActionName() + " script action:\n" + pe);
            return;
        }
        
        if (isScript) {
        	ScriptRecorder.getReference().addLine(getActionName(), parameters);
        } else {
        	ProvenanceRecorder.getReference().addLine(getActionName(), parameters);
        }
    }

    /**
     * {@inheritDoc}
     */
    public void scriptRun(ParameterTable parameters) {
        ViewUserInterface.getReference().createBlankImage(getRawFileInfo(parameters));
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
     *
     * @return  The file info necessary to read in the RAW file.
     */
    public static final FileInfoBase getRawFileInfo(ParameterTable parameters) {
        String imgDir = ViewUserInterface.getReference().getDefaultDirectory();
        String imgFile = "new_blank_image.raw";
        FileInfoBase fileInfo = new FileInfoImageXML(imgFile, imgDir, FileUtility.RAW);

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
