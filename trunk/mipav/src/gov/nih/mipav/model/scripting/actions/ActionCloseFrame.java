package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.dialogs.AlgorithmParameters;


/**
 * An action which closes the frame containing an image.
 */
public class ActionCloseFrame implements ScriptableActionInterface {

    /**
     * The label to use for the input image parameter.
     */
    private static final String INPUT_IMAGE_LABEL = AlgorithmParameters.getInputImageLabel(1);
    
    /**
     * The image whose closing should be recorded in the script.  The actual closing must be done elsewhere.
     */
    private ModelImage recordingInputImage;
    
    /**
     * Constructor for the dynamic instantiation and execution of the CloseFrame script action.
     */
    public ActionCloseFrame() {}
    
    /**
     * Constructor used to record the CloseFrame script action line.
     * @param image  The image whose frame was closed.
     */
    public ActionCloseFrame(ModelImage image) {
        recordingInputImage = image;
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
            MipavUtil.displayError("Error encountered creating input image parameter while recording CloseFrame script action:\n" + pe);
            return;
        }
        
        ScriptRecorder.getReference().addLine("CloseFrame", parameters);
    }

    /**
     * {@inheritDoc}
     */
    public void scriptRun(ParameterTable parameters) {
        parameters.getImage(INPUT_IMAGE_LABEL).getParentFrame().close();
    }
    
    /**
     * Changes the image whose closing should be recorded in the script.
     * @param inputImage  The image whose frame was closed.
     */
    public void setInputImage(ModelImage inputImage) {
        recordingInputImage = inputImage;
    }
}
