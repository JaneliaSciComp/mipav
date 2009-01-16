package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.provenance.ProvenanceRecorder;
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
            parameters.put(createInputImageParameter(isScript));
            addSaveOptionsToParameters(parameters, recordingOptions, recordingInputImage.getExtents());
        } catch (ParserException pe) {
            MipavUtil.displayError("Error encountered creating input image parameter while recording " + getActionName() + " script action:\n" + pe);
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
        ModelImage image = parameters.getImage(INPUT_IMAGE_LABEL);
        
        FileWriteOptions options = getSaveImageOptions(parameters, image, true);
        image.getParentFrame().saveImageInfo();
        image.getParentFrame().save(options, -1);
    }
}