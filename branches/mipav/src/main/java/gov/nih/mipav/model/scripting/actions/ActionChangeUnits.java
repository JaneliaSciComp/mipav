package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;


/**
 * A script action which changes the image's resolutions
 */
public class ActionChangeUnits extends ActionImageProcessorBase {

	/**
     * The label to use for the parameter indicating the new units
     */
    private static final String IMAGE_UNITS = "image_units";
      
    /**
     * Constructor for the dynamic instantiation and execution of the script action.
     */
    public ActionChangeUnits() {
        super();
    }
    
    /**
     * Main constructor with parameters for changing the units of measure
     * @param image the input image
     */
    public ActionChangeUnits(ModelImage image) {
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
            parameters.put(ParameterFactory.newParameter(IMAGE_UNITS, recordingInputImage.getUnitsOfMeasure()));
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
     
        int units [] = parameters.getList(IMAGE_UNITS).getAsIntArray();
        
        int nDims = inputImage.getNDims();
        
        int zDim = 1;
		if (nDims > 2) {
			zDim = inputImage.getExtents()[2];
		}
		if (nDims == 4) {
			zDim *= inputImage.getExtents()[3];
		}
		for (int i = 0; i < zDim; i++) {
			inputImage.getFileInfo()[i].setUnitsOfMeasure(units);
		}
		        
    }
}
