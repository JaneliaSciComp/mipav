package gov.nih.mipav.model.provenance.actions;


import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.actions.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;


/**
 * An action for the mipav system data provenance that records the opening of an image
 */
public class ActionOpenImage extends ActionImageProcessorBase {
    
    
    //~ Methods --------------------------------------------------------------------------------------------------------

	/**
     * Constructor used to record the provenance action line
     * @param input  The image which was processed.
     */
    public ActionOpenImage(ModelImage input) {
        super(input);
    }
	
    /**
     * {@inheritDoc}
     */
    public void insertScriptLine() {
    	ParameterTable parameters = new ParameterTable();
    	try {
            parameters.put(createInputImageParameter(isScript));
        } catch (ParserException pe) {
            MipavUtil.displayError("Error encountered creating input image parameter while recording " + getActionName() + " script action:\n" + pe);
            return;
        }
        
        if (isScript) {
           return;
        } else {
        	ProvenanceRecorder.getReference().addLine(getActionName(), parameters);
        }
    }
    
    public void scriptRun(ParameterTable p) {
    	//not used in script
    }
}
