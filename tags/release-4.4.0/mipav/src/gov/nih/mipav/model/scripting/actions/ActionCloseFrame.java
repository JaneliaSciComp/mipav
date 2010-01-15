package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;


/**
 * An action which closes the frame containing an image.
 */
public class ActionCloseFrame extends ActionImageProcessorBase {
    /**
     * Constructor for the dynamic instantiation and execution of the script action.
     */
    public ActionCloseFrame() {
        super();
    }
    
    /**
     * Constructor used to record the script action line.
     * @param input  The image which was processed.
     */
    public ActionCloseFrame(ModelImage input) {
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
            MipavUtil.displayError("Error encountered creating input image parameter while recording " + getActionName() + " script action:\n" + pe);
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
        parameters.getImage(INPUT_IMAGE_LABEL).getParentFrame().close();
    }
}
