package gov.nih.mipav.model.scripting.actions;


import java.awt.event.ActionEvent;

import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;


/**
 * A script action which generates a VOI based on the paint mask of an image.
 */
public class ActionPaintToVOI extends ActionImageProcessorBase {

    /**
     * Constructor for the dynamic instantiation and execution of the PaintToVOI script action.
     */
    public ActionPaintToVOI() {
        super();
    }
    
    /**
     * Constructor used to record the PaintToVOI script action line.
     * 
     * @param  inputImage  The image containg the paint mask which was used to generate the VOI.
     */
    public ActionPaintToVOI(ModelImage inputImage) {
        super(inputImage);
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
        ModelImage inputImage = parameters.getImage(INPUT_IMAGE_LABEL);
        ViewJFrameImage frame = inputImage.getParentFrame();

        frame.actionPerformed(new ActionEvent(frame, 0, "PaintToVOI"));
    }
}
