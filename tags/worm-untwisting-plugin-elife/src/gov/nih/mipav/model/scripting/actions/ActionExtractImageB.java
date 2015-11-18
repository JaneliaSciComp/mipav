package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;


/**
 * An action which extracts the imageB from a frame and puts it into a new image frame.
 */
public class ActionExtractImageB extends ActionImageProcessorBase {

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for the dynamic instantiation and execution of the script action.
     */
    public ActionExtractImageB() {
        super();
    }

    /**
     * Constructor used to record the script action line.
     *
     * @param  input   The image which was processed.
     * @param  result  The result image generated.
     */
    public ActionExtractImageB(ModelImage input, ModelImage result) {
        super(input, result);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
    public void insertScriptLine() {
        ParameterTable parameters = new ParameterTable();

        try {
            parameters.put(createInputImageParameter(isScript));
            storeImageInRecorder(recordingResultImage, isScript);
        } catch (ParserException pe) {
            MipavUtil.displayError("Error encountered creating input image parameter while recording " +
                                   getActionName() + " script action:\n" + pe);

            return;
        }

        ScriptRecorder.getReference().addLine(getActionName(), parameters);
    }

    /**
     * {@inheritDoc}
     */
    public void scriptRun(ParameterTable parameters) {
        ModelImage clonedImage = null;
        ViewJFrameImage frame = parameters.getImage(INPUT_IMAGE_LABEL).getParentFrame();

        try {
            clonedImage = (ModelImage) frame.getImageB().clone();

            ModelLUT lutClone = null;

            lutClone = (ModelLUT) frame.getLUTb().clone();

            new ViewJFrameImage(clonedImage, lutClone, new Dimension(610, 200), frame.getImageB().getLogMagDisplay());

            System.gc();

            ScriptRunner.getReference().getImageTable().storeImageName(clonedImage.getImageName());
        } catch (OutOfMemoryError error) {

            if (clonedImage != null) {
                clonedImage.disposeLocal();
            }

            clonedImage = null;
            System.gc();
            MipavUtil.displayError("Out of memory: unable to open new frame");

            return;
        }
    }
}
