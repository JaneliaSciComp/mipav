package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.AlgorithmParameters;


/**
 * A script action which opens all VOIs from the directory where VOIs are saved by default when a SaveAllVOIs action is executed.
 */
public class ActionOpenAllVOIs implements ScriptableActionInterface {

    /**
     * The label to use for the input image parameter.
     */
    private static final String INPUT_IMAGE_LABEL = AlgorithmParameters.getInputImageLabel(1);
    
    /**
     * The image whose VOI-opening should be recorded in the script.  The actual VOI-opening must be done elsewhere.
     */
    private ModelImage recordingInputImage;
    
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
            parameters.put(ParameterFactory.newImage(INPUT_IMAGE_LABEL, recordingInputImage.getImageName()));
        } catch (ParserException pe) {
            MipavUtil.displayError("Error encountered creating parameters while recording OpenAllVOIs script action:\n" + pe);
            return;
        }
        
        ScriptRecorder.getReference().addLine("OpenAllVOIs", parameters);
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
        component.getVOIHandler().fireVOISelectionChange(loadedVOI, (VOIBase) loadedVOI.getCurves()[component.getSlice()].elementAt(0));
    }
    
    /**
     * Changes the image whose VOI-opening should be recorded in the script.
     * @param inputImage  The image whose VOIs were opened.
     */
    public void setInputImage(ModelImage inputImage) {
        recordingInputImage = inputImage;
    }
}
