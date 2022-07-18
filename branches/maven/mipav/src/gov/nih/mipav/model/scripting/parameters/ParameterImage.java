package gov.nih.mipav.model.scripting.parameters;


import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.ViewJFrameImage;


/**
 * A image placeholder variable parameter used in either the recording or execution of a script action.
 */
public class ParameterImage extends ParameterString {

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ParameterImage object.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * 
     * @throws ParserException If there is a problem creating the parameter.
     */
    public ParameterImage(final String paramLabel) throws ParserException {
        super(paramLabel, Parameter.PARAM_IMAGE);
    }

    /**
     * Creates a new ParameterImage object.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * @param paramType The type of this parameter (should be PARAM_IMAGE).
     * 
     * @throws ParserException If there is a problem creating the parameter.
     */
    public ParameterImage(final String paramLabel, final int paramType) throws ParserException {
        super(paramLabel, paramType);
    }

    /**
     * Creates a new ParameterImage object.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * @param paramTypeString The type of this parameter, in string form.
     * @param paramValueString The new parameter value.
     * 
     * @throws ParserException If there is a problem creating the parameter.
     */
    public ParameterImage(final String paramLabel, final String paramTypeString, final String paramValueString)
            throws ParserException {
        super(paramLabel, paramTypeString, paramValueString);
    }

    /**
     * Creates a new ParameterImage object.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * @param paramType The type of this parameter (should be PARAM_IMAGE).
     * @param paramValueString The new parameter value.
     * 
     * @throws ParserException If there is a problem creating the parameter.
     */
    public ParameterImage(final String paramLabel, final int paramType, final String paramValueString)
            throws ParserException {
        super(paramLabel, paramType, paramValueString);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Return the frame containing the image that this image placeholder refers to.
     * 
     * @return The frame containing the image assigned to this image placeholder variable.
     */
    public ViewJFrameImage getFrame() {
        return getImage().getParentFrame();
    }

    /**
     * Return the image that this image placeholder refers to.
     * 
     * @return The image assigned to this image placeholder variable.
     */
    public ModelImage getImage() {
        return ScriptRunner.getReference().getImage(getValue());
    }

    /**
     * Checks to see if this image placeholder variable is the same as another image placeholder (e.g., '$image1' ==
     * '$image1'; does not check image name).
     * 
     * @param secondImageParam Another image variable parameter.
     * 
     * @return <code>True</code> if the parameters have the same image placeholder, <code>false</code> otherwise.
     */
    public boolean isSameImageAs(final ParameterImage secondImageParam) {
        return getValue().equals(secondImageParam.getValue());
    }
}
