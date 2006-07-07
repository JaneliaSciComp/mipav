package gov.nih.mipav.model.scripting.actions;


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
     * @param input  The image which was processed.
     */
    public ActionClone(ModelImage input) {
        super(input);
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
    public void insertScriptLine() {
        ParameterTable parameters = new ParameterTable();
        try {
            parameters.put(createInputImageParameter());
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
        ModelImage inputImage = parameters.getImage(INPUT_IMAGE_LABEL);
        ModelImage clonedImage = (ModelImage) inputImage.clone();
        ScriptRunner.getReference().getImageTable().storeImageName(clonedImage.getImageName());
        new ViewJFrameImage(clonedImage);
    }
}
