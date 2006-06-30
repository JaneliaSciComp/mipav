package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.dialogs.AlgorithmParameters;


/**
 * A script action which clones an input image and puts it into a new image frame.
 */
public class ActionClone implements ScriptableActionInterface {

    /**
     * The label to use for the input image parameter.
     */
    private static final String INPUT_IMAGE_LABEL = AlgorithmParameters.getInputImageLabel(1);
    
    /**
     * The image whose cloning should be recorded in the script.  The actual cloning must be done elsewhere.
     */
    private ModelImage recordingInputImage;
    
    /**
     * Constructor for the dynamic instantiation and execution of the Clone script action.
     */
    public ActionClone() {}
    
    /**
     * Constructor used to record the Clone script action line.
     * @param input  The image which was cloned.
     */
    public ActionClone(ModelImage input) {
        recordingInputImage = input;
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
    public void insertScriptLine() {
        ParameterTable parameters = new ParameterTable();
        try {
            parameters.put(ParameterFactory.newImage(INPUT_IMAGE_LABEL, recordingInputImage.getImageName()));
        } catch (ParserException pe) {
            MipavUtil.displayError("Error encountered creating input image parameter while recording Clone script action:\n" + pe);
            return;
        }
        
        ScriptRecorder.getReference().addLine("Clone", parameters);
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
    
    /**
     * Changes the image whose cloning should be recorded in the script.
     * @param inputImage  The image which was cloned.
     */
    public void setInputImage(ModelImage inputImage) {
        recordingInputImage = inputImage;
    }
}
