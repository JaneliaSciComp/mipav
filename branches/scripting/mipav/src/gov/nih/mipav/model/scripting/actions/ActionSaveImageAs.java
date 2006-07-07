package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.ScriptRecorder;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;


/**
 * A script action which writes out an image to disk.
 */
public class ActionSaveImageAs extends ActionSaveBase {
    /**
     * The name of the parameter variable which allows the user to specify the name to save the image as from the command line (or the script run dialog, possibly).
     */
    protected static final String SAVE_AS_FILE_NAME = "$save_as_file_name";
    
    /**
     * Constructor for the dynamic instantiation and execution of the SaveImage script action.
     */
    public ActionSaveImageAs() {
        super();
    }
    
    /**
     * Constructor used to record the script action line.
     * @param input    The image which was saved.
     * @param options  The options used during the image save.
     */
    public ActionSaveImageAs(ModelImage input, FileWriteOptions options) {
        super(input, options);
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
    public void insertScriptLine() {
        ParameterTable parameters = new ParameterTable();
        try {
            parameters.put(createInputImageParameter());
            addSaveOptionsToParameters(parameters, recordingOptions, recordingInputImage.getExtents());
        } catch (ParserException pe) {
            MipavUtil.displayError("Error encountered creating input image parameter while recording " + getActionName() + " script action:\n" + pe);
            return;
        }
        
        ScriptRecorder.getReference().addLine(getActionName(), parameters);
    }

    /**
     * {@inheritDoc}
     */
    public void scriptRun(ParameterTable parameters) {
        ModelImage image = parameters.getImage(INPUT_IMAGE_LABEL);
        
        String saveImageFileName = parameters.getVariable(SAVE_AS_FILE_NAME).getValue();
        
        // TODO: get the savedImageFileName from the -o option on the command line?  maybe have commandline opts that set the values of ParameterVariables (and use one for the file name)
        if (saveImageFileName != null) {
            FileWriteOptions options = getSaveImageOptions(parameters, image, true);
            options.setFileName(saveImageFileName);
            image.getParentFrame().save(options, -1);
        } else {
            image.getParentFrame().saveImageInfo();
            image.getParentFrame().save(getSaveImageOptions(parameters, image, true), -1);
        }
    }
}