package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.MipavUtil;


/**
 * A script action which changes the name of an image.
 */
public class ActionChangeName extends ActionImageProcessorBase {
    /**
     * The value that can be used with the 'image_name_action' parameter to indicate that the current image name should
     * be replaced with the 'new_image_name' parameter value.
     */
    public static final String NAME_ACTION_REPLACE = "replace";

    /**
     * The value that can be used with the 'image_name_action' parameter to indicate that the 'new_image_name' parameter
     * should be prepended to the current image name.
     */
    public static final String NAME_ACTION_PREPEND = "prepend";

    /**
     * The value that can be used with the 'image_name_action' parameter to indicate that the 'new_image_name' parameter
     * should be appended to the current image name.
     */
    public static final String NAME_ACTION_APPEND = "append";

    /**
     * The label to use for the parameter indicating the new image name.
     */
    private static final String IMAGE_NAME_LABEL = "new_image_name";

    /**
     * The label to use for the parameter indicating whether the image name should be replaced, prepended to, or
     * appended to.
     */
    private static final String IMAGE_NAME_ACTION_LABEL = "image_name_action";

    /**
     * The new name given to the image.
     */
    private String recordingNewImageName;

    /**
     * The old image name.
     */
    private String recordingOldImageName;

    /**
     * Constructor for the dynamic instantiation and execution of the script action.
     */
    public ActionChangeName() {
        super();
    }

    /**
     * Constructor used to record the ChangeName script action line.
     * 
     * @param image The image whose name was changed.
     * @param oldImageName The old name of the image.
     * @param newImageName The new name given to the image (the image's current name).
     */
    public ActionChangeName(ModelImage image, String oldImageName, String newImageName) {
        super(image);
        recordingOldImageName = oldImageName;
        recordingNewImageName = newImageName;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
    public void insertScriptLine() {
        // change the image name stored in the image table if it has been used in the script before.
        // if it hasn't been used before storing it will be handled in createInputImageParameter(isScript)
        ImageVariableTable imageTable = null;
        if (isScript) {
            imageTable = ScriptRecorder.getReference().getImageTable();
        } else {
            imageTable = ProvenanceRecorder.getReference().getImageTable();
        }

        if (imageTable.isImageStored(recordingOldImageName)) {
            imageTable.changeImageName(recordingOldImageName, recordingNewImageName);
        }

        ParameterTable parameters = new ParameterTable();
        try {
            parameters.put(createInputImageParameter(isScript));
            parameters.put(ParameterFactory.newString(IMAGE_NAME_LABEL, recordingNewImageName));
            // default to always clobber the image name, but leave it in so that the user can change it
            parameters.put(ParameterFactory.newString(IMAGE_NAME_ACTION_LABEL, NAME_ACTION_REPLACE));
        } catch (ParserException pe) {
            MipavUtil.displayError("Error encountered while recording " + getActionName() + " script action:\n" + pe);
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

        String oldImageName = inputImage.getImageName();
        String newImageName = parameters.getString(IMAGE_NAME_LABEL);

        // optional parameter allowing appending the string to the end of the current image name
        String imageNameAction = NAME_ACTION_REPLACE;
        if (parameters.containsParameter(IMAGE_NAME_ACTION_LABEL)) {
            imageNameAction = parameters.getString(IMAGE_NAME_ACTION_LABEL);
        }

        if (imageNameAction.equals(NAME_ACTION_PREPEND)) {
            newImageName = newImageName + oldImageName;
        } else if (imageNameAction.equals(NAME_ACTION_APPEND)) {
            newImageName = oldImageName + newImageName;
        }

        inputImage.updateFileName(newImageName);

        ScriptRunner.getReference().getImageTable().changeImageName(oldImageName, newImageName);
    }

    /**
     * Changes the image name which should be recorded as given to the input image.
     * 
     * @param name The new name given to the input image.
     */
    public void setNewImageName(String name) {
        recordingNewImageName = name;
    }

    /**
     * Changes the old name of the input image.
     * 
     * @param name The old name of the input image.
     */
    public void setOldImageName(String name) {
        recordingOldImageName = name;
    }
}
