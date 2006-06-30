package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewUserInterface;


/**
 * A script action which creates a new blank image with a set of characteristics.
 */
public class ActionCreateBlankImage implements ScriptableActionInterface {
    /**
     * The blank image whose creation should be recorded in the script.  The actual creation must be done elsewhere.
     */
    private ModelImage recordingBlankImage;
    
    /**
     * Constructor for the dynamic instantiation and execution of the CreateBlankImage script action.
     */
    public ActionCreateBlankImage() {}
    
    /**
     * Constructor used to record the CreateBlankImage script action line.
     * @param input  The blank image which was created.
     */
    public ActionCreateBlankImage(ModelImage input) {
        recordingBlankImage = input;
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
    public void insertScriptLine() {
        ParameterTable parameters = new ParameterTable();
        try {
            ActionOpenImage.addRawOptionsToParameters(parameters, recordingBlankImage.getFileInfo(0));
        } catch (ParserException pe) {
            MipavUtil.displayError("Error encountered creating parameters while recording CreateBlankImage script action:\n" + pe);
            return;
        }
        
        ScriptRecorder.getReference().addLine("CreateBlankImage", parameters);
    }

    /**
     * {@inheritDoc}
     */
    public void scriptRun(ParameterTable parameters) {
        ViewUserInterface.getReference().createBlankImage(ActionOpenImage.getRawFileInfo(parameters, false));
    }
    
    /**
     * Changes the blank image whose creation should be recorded in the script.
     * @param blankImage  The image which was created.
     */
    public void setBlankImage(ModelImage blankImage) {
        recordingBlankImage = blankImage;
    }
}
