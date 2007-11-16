package gov.nih.mipav.model.provenance.actions;


import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.actions.*;
import gov.nih.mipav.model.scripting.parameters.*;


/**
 * An action for the mipav system data provenance that records the starting of MIPAV
 */
public class ActionStartMipav extends ActionImageProcessorBase {
    
    
    //~ Methods --------------------------------------------------------------------------------------------------------

	
    /**
     * {@inheritDoc}
     */
    public void insertScriptLine() {
    	ParameterTable parameters = new ParameterTable();
    
        if (isScript) {
           return;
        } else {
        	ProvenanceRecorder.getReference().addLine(getActionName(), parameters);
        }
    }
    
    /**
     * unused
     */
    public void scriptRun(ParameterTable p) {
    	//not used in script
    }
}
