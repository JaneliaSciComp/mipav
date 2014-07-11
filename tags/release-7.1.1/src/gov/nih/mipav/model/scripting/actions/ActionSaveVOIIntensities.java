package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;


/**
 * A script action which saves all the VOI intensities in an image to disk.
 */
public class ActionSaveVOIIntensities extends ActionImageProcessorBase {

    /**
     * The label to use for the VOI Intensities save destination directory parameter (leave empty for the default directory).
     */
    private static final String SAVE_VOI_INTENSITIES_TO_DIR = "save_voi_intensities_to_dir";
    
    /**
     * The directory where the VOI intensities were saved to, which should be recorded in the script.  Leave empty or null to save in the current default directory.
     */
    private String recordingTargetDir;
    
    /**
     * Constructor for the dynamic instantiation and execution of the SaveVOIIntensities script action.
     */
    public ActionSaveVOIIntensities() {
        super();
    }
    
    /**
     * Constructor used to record the SaveVOIIntensities script action line.
     * 
     * @param  input  The image which had its VOI intensities saved.
     */
    public ActionSaveVOIIntensities(ModelImage input) {
        super(input);
    }
    
    /**
     * Constructor used to record the SaveVOIIntensities script action line.
     * 
     * @param  input      The image which had its VOI intensities saved.
     * @param  targetDir  The directory where the VOI intensities were saved (leave empty or null for the default directory).
     */
    public ActionSaveVOIIntensities(ModelImage input, String targetDir) {
        this(input);
        
        recordingTargetDir = targetDir;
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
    public void insertScriptLine() {
        ParameterTable parameters = new ParameterTable();
        try {
            parameters.put(createInputImageParameter(isScript));
            if (recordingTargetDir == null) {
                recordingTargetDir = "";
            }
            parameters.put(ParameterFactory.newString(SAVE_VOI_INTENSITIES_TO_DIR, recordingTargetDir));
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
        String targetDir = parameters.getString(SAVE_VOI_INTENSITIES_TO_DIR);
        
        if (targetDir == null || targetDir.equals("")) {
            parameters.getImage(INPUT_IMAGE_LABEL).getParentFrame().saveVOIIntensities();
        } else {
            parameters.getImage(INPUT_IMAGE_LABEL).getParentFrame().saveVOIIntensitiesTo(targetDir);
        }
    }
    
    /**
     * Changes the target directory where the VOI intensities were saved, which should be recorded in the script.  Use empty or null string to indicate that the default directory should be used.
     * @param targetDir  The directory where the VOI intensities were saved.
     */
    public void setTargetDirectory(String targetDir) {
        recordingTargetDir = targetDir;
    }
}
