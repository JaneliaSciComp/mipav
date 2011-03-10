package gov.nih.mipav.model.scripting.actions;

import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.ScriptRecorder;

import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;

public class ActionSelectAllVOIs extends ActionImageProcessorBase {

	
	
	/**
     * Constructor for the dynamic instantiation and execution of the SelectAllVOIs script action.
     */
    public ActionSelectAllVOIs() {
        super();
    }
    
    /**
     * Constructor used to record the SelectAllVOIs script action line.
     * 
     * @param  input  The image which had its VOIs saved.
     */
    public ActionSelectAllVOIs(ModelImage input) {
        super(input);
    }
	
	
	
	
	public void insertScriptLine() {
		 ParameterTable parameters = new ParameterTable();
	        try {
	            parameters.put(createInputImageParameter(isScript));

	        } catch (ParserException pe) {
	            MipavUtil.displayError("Error encountered creating parameters while recording " + getActionName() + " script action:\n" + pe);
	            return;
	        }
	        
	        if (isScript) {
	        	ScriptRecorder.getReference().addLine(getActionName(), parameters);
	        } else {
	        	ProvenanceRecorder.getReference().addLine(getActionName(), parameters);
	        }

	}

	public void scriptRun(ParameterTable parameters) {
		 parameters.getImage(INPUT_IMAGE_LABEL).getParentFrame().getVOIManager().selectAllVOIs(true);

	}

}
