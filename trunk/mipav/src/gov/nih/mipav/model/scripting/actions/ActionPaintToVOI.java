package gov.nih.mipav.model.scripting.actions;


import java.awt.event.ActionEvent;

import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.dialogs.AlgorithmParameters;


/**
 * A script action which generates a VOI based on the paint mask of an image.
 */
public class ActionPaintToVOI implements ScriptableActionInterface {

    /**
     * The label to use for the input image parameter.
     */
    private static final String INPUT_IMAGE_LABEL = AlgorithmParameters.getInputImageLabel(1);
    
    /**
     * The image containing the paint mask used to generate a VOI (which should now be recorded).
     */
    private ModelImage recordingInputImage;

    /**
     * Constructor for the dynamic instantiation and execution of the PaintToVOI script action.
     */
    public ActionPaintToVOI() {}
    
    /**
     * Constructor used to record the PaintToVOI script action line.
     * @param inputImage  The image containg the paint mask which was used to generate the VOI.
     */
    public ActionPaintToVOI(ModelImage inputImage) {
        recordingInputImage = inputImage;
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
    public void insertScriptLine() {
        ParameterTable parameters = new ParameterTable();
        try {
            parameters.put(ParameterFactory.newImage(INPUT_IMAGE_LABEL, recordingInputImage.getImageName()));
        } catch (ParserException pe) {
            MipavUtil.displayError("Error encountered creating input image parameter while recording PaintToVOI script action:\n" + pe);
            return;
        }
        
        ScriptRecorder.getReference().addLine("PaintToVOI", parameters);
    }

    /**
     * {@inheritDoc}
     */
    public void scriptRun(ParameterTable parameters) {
        ModelImage inputImage = parameters.getImage(INPUT_IMAGE_LABEL);
        ViewJFrameImage frame = inputImage.getParentFrame();

        frame.actionPerformed(new ActionEvent(frame, 0, "PaintToVOI"));
    }
    
    /**
     * Changes the image containing the paint mask used to generate a new VOI.
     * @param inputImage  The image containing the paint mask used to generate a VOI.
     */
    public void setInputImage(ModelImage inputImage) {
        recordingInputImage = inputImage;
    }
}
