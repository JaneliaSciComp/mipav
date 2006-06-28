package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.dialogs.AlgorithmParameters;


/**
 * A script action which changes the name of an image.
 */
public class ActionChangeName implements ScriptableActionInterface {

    /**
     * The label to use for the input image parameter.
     */
    private static final String INPUT_IMAGE_LABEL = AlgorithmParameters.getInputImageLabel(1);
    
    /**
     * The label to use for the parameter indicating the new image name.
     */
    private static final String IMAGE_NAME_LABEL = "new_image_name";
    
    /**
     * The image whose name change should be recorded in the script.  The actual name change must be done elsewhere (after the recording).
     */
    private ModelImage recordingInputImage;
    
    /**
     * The new name given to the image
     */
    private String recordingImageName;
    
    /**
     * Constructor for the dynamic instantiation and execution of the ChangeName script action.
     */
    public ActionChangeName() {}
    
    /**
     * Constructor used to record the ChangeName script action line.
     * @param  image         The image whose name was changed.
     * @param  newImageName  The new name given to the image.
     */
    public ActionChangeName(ModelImage image, String newImageName) {
        recordingInputImage = image;
        recordingImageName = newImageName;
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
    public void insertScriptLine() {
        ParameterTable parameters = new ParameterTable();
        try {
            parameters.put(ParameterFactory.newImage(INPUT_IMAGE_LABEL, recordingInputImage.getImageName()));
            parameters.put(ParameterFactory.newString(IMAGE_NAME_LABEL, recordingImageName));
        } catch (ParserException pe) {
            MipavUtil.displayError("Error encountered while recording ChangeName script action:\n" + pe);
            return;
        }
        
        ScriptRecorder.getReference().addLine("ChangeName", parameters);
    }

    /**
     * {@inheritDoc}
     */
    public void scriptRun(ParameterTable parameters) {
        ModelImage inputImage = parameters.getImage(INPUT_IMAGE_LABEL);
        String inputImagePlaceholder = parameters.get(INPUT_IMAGE_LABEL).getValueString();
        String newImageName = parameters.getString(IMAGE_NAME_LABEL);
        
        inputImage.updateFileName(newImageName);
        VariableTable.getReference().removeVariable(inputImagePlaceholder);
        VariableTable.getReference().storeVariable(inputImagePlaceholder, newImageName);
    }
    
    /**
     * Changes the image whose name change should be recorded in the script.
     * @param inputImage  The image whose name was changed.
     */
    public void setInputImage(ModelImage inputImage) {
        recordingInputImage = inputImage;
    }
    
    /**
     * Changes the image name which should be recorded as given to the input image.
     * @param name  The new name given to the input image.
     */
    public void setNewImageName(String name) {
        recordingImageName = name;
    }
}
