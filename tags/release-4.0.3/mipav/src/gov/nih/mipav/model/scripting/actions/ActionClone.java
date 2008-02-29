package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;


/**
 * A script action which clones an input image and puts it into a new image frame.
 */
public class ActionClone extends ActionImageProcessorBase {
    
    /**
     * Constructor for the dynamic instantiation and execution of the script action.
     */
    public ActionClone() {
        super();
    }
    
    /**
     * Constructor used to record the script action line.
     * 
     * @param  input   The image which was processed.
     * @param  result  The result image generated.
     */
    public ActionClone(ModelImage input, ModelImage result) {
        super(input, result);
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
    public void insertScriptLine() {
        ParameterTable parameters = new ParameterTable();
        try {
            parameters.put(createInputImageParameter(isScript));
            storeImageInRecorder(recordingResultImage, isScript);
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
        ModelImage inputImage = parameters.getImage(INPUT_IMAGE_LABEL);
        ModelImage clonedImage = (ModelImage) inputImage.clone();
        ScriptRunner.getReference().getImageTable().storeImageName(clonedImage.getImageName());
        new ViewJFrameImage(clonedImage);
    }
}
