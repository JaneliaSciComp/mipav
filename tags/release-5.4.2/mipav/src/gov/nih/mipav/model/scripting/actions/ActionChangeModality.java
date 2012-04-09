package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;


/**
 * A script action which changes the image's modality
 */
public class ActionChangeModality extends ActionImageProcessorBase {

	/**
     * The label to use for the parameter indicating the new units
     */
    private static final String MODALITY = "modality";
      
    /**
     * Constructor for the dynamic instantiation and execution of the script action.
     */
    public ActionChangeModality() {
        super();
    }
    
    /**
     * Main constructor with parameters for changing the modality
     * @param image the input image
     */
    public ActionChangeModality(ModelImage image) {
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
            parameters.put(ParameterFactory.newParameter(MODALITY, recordingInputImage.getFileInfo(0).getModality()));
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
     
        int modality = parameters.getInt(MODALITY);
        
        if (inputImage.getNDims() == 2) { // source image is 2D

        	inputImage.getFileInfo(0).setModality(modality);
        } else if (inputImage.getNDims() == 3) {
            for (int n = 0; n < inputImage.getExtents()[2]; n++) {
            	inputImage.getFileInfo(n).setModality(modality);
            }
        } else {

            for (int n = 0; n < (inputImage.getExtents()[2] * inputImage.getExtents()[3]); n++) {
            	inputImage.getFileInfo(n).setModality(modality);
            }
        }
		        
    }
}
