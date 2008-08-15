package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;


/**
 * A script action which changes the image's orientations
 */
public class ActionChangeOrientations extends ActionImageProcessorBase {

	/**
     * The label to use for the parameter indicating the new orientation
     */
    private static final String IMAGE_ORIENTATION = "image_orientation";
      
    /**
     * The label to use for the parameter indicating the new axis orientations
     */
    private static final String AXIS_ORIENTATIONS = "axis_orientations";
    
    /**
     * Constructor for the dynamic instantiation and execution of the script action.
     */
    public ActionChangeOrientations() {
        super();
    }
    
    /**
     * Main constructor with parameters for changing the orientations
     * @param image the input image
     */
    public ActionChangeOrientations(ModelImage image) {
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
            parameters.put(ParameterFactory.newParameter(IMAGE_ORIENTATION, recordingInputImage.getImageOrientation()));
            if (recordingInputImage.getNDims() > 2) {
            	parameters.put(ParameterFactory.newParameter(AXIS_ORIENTATIONS, recordingInputImage.getFileInfo()[0].getAxisOrientation()));
            }
            
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
     
        int orientation = parameters.getInt(IMAGE_ORIENTATION);
        
        inputImage.setImageOrientation(orientation);
        
        int [] axisOrient = null;
        
        if (inputImage.getNDims() > 2) {
        	axisOrient = parameters.getList(AXIS_ORIENTATIONS).getAsIntArray();
        	
        	for (int i = 0; i < inputImage.getFileInfo().length; i++) {
        		inputImage.getFileInfo()[i].setAxisOrientation(axisOrient);
        	}	
        }
    }
}
