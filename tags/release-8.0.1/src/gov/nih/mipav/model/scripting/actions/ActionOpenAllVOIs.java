package gov.nih.mipav.model.scripting.actions;

import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;


/**
 * A script action which opens all VOIs from the directory where VOIs are saved by default when a SaveAllVOIs action is executed.
 */
public class ActionOpenAllVOIs extends ActionImageProcessorBase {
    /**
     * Constructor for the dynamic instantiation and execution of the OpenAllVOIs script action.
     */
    public ActionOpenAllVOIs() {}
    
    /**
     * Constructor used to record the OpenAllVOIs script action line.
     * @param input  The image which had its VOIs opened.
     */
    public ActionOpenAllVOIs(ModelImage input) {
        recordingInputImage = input;
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
        ModelImage inputImage = parameters.getImage(INPUT_IMAGE_LABEL);
        
        inputImage.getParentFrame().loadAllVOIs(false);
        
        ViewJComponentEditImage component = inputImage.getParentFrame().getComponentImage();
        VOI loadedVOI = inputImage.getVOIs().VOIAt(0);
        
        loadedVOI.setAllActive(true);
        component.getVOIHandler().setVOI_IDs(loadedVOI.getID(), loadedVOI.getUID());
        component.getVOIHandler().fireVOISelectionChange(loadedVOI);
    }
}
