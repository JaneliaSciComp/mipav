package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.dialogs.AlgorithmParameters;


/**
 * A script action which saves all the VOIs in an image to disk.
 */
public class ActionSaveAllVOIs implements ScriptableActionInterface {

    /**
     * The label to use for the input image parameter.
     */
    private static final String INPUT_IMAGE_LABEL = AlgorithmParameters.getInputImageLabel(1);
    
    /**
     * The label to use for the VOI-save destination directory parameter (leave empty for the default directory).
     */
    private static final String SAVE_VOIS_TO_DIR = "save_vois_to_dir";
    
    /**
     * The image whose VOI-saving should be recorded in the script.  The actual VOI-saving must be done elsewhere.
     */
    private ModelImage recordingInputImage;
    
    /**
     * The directory where the VOIs were saved to, which should be recorded in the script.  Leave empty or null to save in the current default directory.
     */
    private String recordingTargetDir;
    
    /**
     * Constructor for the dynamic instantiation and execution of the SaveAllVOIs script action.
     */
    public ActionSaveAllVOIs() {}
    
    /**
     * Constructor used to record the SaveAllVOIs script action line.
     * @param input  The image which had its VOIs saved.
     */
    public ActionSaveAllVOIs(ModelImage input) {
        recordingInputImage = input;
    }
    
    /**
     * Constructor used to record the SaveAllVOIs script action line.
     * @param input      The image which had its VOIs saved.
     * @param targetDir  The directory where the VOIs were saved (leave empty or null for the default directory).
     */
    public ActionSaveAllVOIs(ModelImage input, String targetDir) {
        recordingInputImage = input;
        recordingTargetDir = targetDir;
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
    public void insertScriptLine() {
        ParameterTable parameters = new ParameterTable();
        try {
            parameters.put(ParameterFactory.newImage(INPUT_IMAGE_LABEL, recordingInputImage.getImageName()));
            if (recordingTargetDir == null) {
                recordingTargetDir = "";
            }
            parameters.put(ParameterFactory.newString(SAVE_VOIS_TO_DIR, recordingTargetDir));
        } catch (ParserException pe) {
            MipavUtil.displayError("Error encountered creating parameters while recording SaveAllVOIs script action:\n" + pe);
            return;
        }
        
        ScriptRecorder.getReference().addLine("SaveAllVOIs", parameters);
    }

    /**
     * {@inheritDoc}
     */
    public void scriptRun(ParameterTable parameters) {
        String targetDir = parameters.getString(SAVE_VOIS_TO_DIR);
        
        if (targetDir == null || targetDir.equals("")) {
            parameters.getImage(INPUT_IMAGE_LABEL).getParentFrame().saveAllVOIs();
        } else {
            parameters.getImage(INPUT_IMAGE_LABEL).getParentFrame().saveAllVOIsTo(targetDir);
        }
    }
    
    /**
     * Changes the image whose VOI-saving should be recorded in the script.
     * @param inputImage  The image whose VOIs were saved.
     */
    public void setInputImage(ModelImage inputImage) {
        recordingInputImage = inputImage;
    }
    
    /**
     * Changes the target directory where the VOIs were saved, which should be recorded in the script.  Use empty or null string to indicate that the default directory should be used.
     * @param targetDir  The directory where the VOIs were saved.
     */
    public void setTargetDirectory(String targetDir) {
        recordingTargetDir = targetDir;
    }
}
