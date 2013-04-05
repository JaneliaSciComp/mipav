package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;

import java.io.IOException;


/**
 * A script action which opens a VOI from disk and adds it to an image.
 */
public class ActionOpenVOI extends ActionImageProcessorBase {
    
    /**
     * Constructor for the dynamic instantiation and execution of the OpenVOI script action.
     */
    public ActionOpenVOI() {
        super();
    }
    
    /**
     * Constructor used to record the OpenVOI script action line.
     * 
     * @param  input  The image to which the VOI was added.
     */
    public ActionOpenVOI(ModelImage input) {
        super(input);
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
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

    /**
     * {@inheritDoc}
     */
    public void scriptRun(ParameterTable parameters) {
        String imageVar = parameters.getImageParameter(INPUT_IMAGE_LABEL).getValue();
        
        try {
            ScriptRunner.getReference().getVOITable().openAndRegisterNextVOI(imageVar);
        } catch (IOException ioe) {
            throw new ParameterException(INPUT_IMAGE_LABEL, "Error encountered loading VOI for image " + imageVar + " from disk.  Script results may be invalid.");
        }
    }
}
