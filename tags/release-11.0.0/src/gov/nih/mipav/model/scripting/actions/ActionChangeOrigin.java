package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;


/**
 * A script action which changes the image's origin
 */
public class ActionChangeOrigin extends ActionImageProcessorBase {

	/**
     * The label to use for the parameter indicating the new origin
     */
    private static final String IMAGE_ORIGIN = "image_origin";
      
    /**
     * Constructor for the dynamic instantiation and execution of the script action.
     */
    public ActionChangeOrigin() {
        super();
    }
    
    /**
     * Main constructor with parameters for changing the origin
     * @param image the input image
     */
    public ActionChangeOrigin(ModelImage image) {
        super(image);
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
    public void insertScriptLine() {
        
        ParameterTable parameters = new ParameterTable();
        try {
        	parameters.put(createInputImageParameter(isScript));
            parameters.put(ParameterFactory.newParameter(IMAGE_ORIGIN, recordingInputImage.getOrigin()));
            
        } catch (ParserException pe) {
            MipavUtil.displayError("Error encountered while recording " + getActionName() + " script action:\n" + pe);
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
       
        float [] origin  = parameters.getList(IMAGE_ORIGIN).getAsFloatArray();
        	
        for (int i = 0; i < inputImage.getFileInfo().length; i++) {
        	inputImage.getFileInfo()[i].setOrigin(origin);
        }	
    
    }
}
