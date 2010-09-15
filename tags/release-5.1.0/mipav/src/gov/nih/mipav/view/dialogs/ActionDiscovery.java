package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.scripting.parameters.ParameterTable;


/**
 * Classes which implement this interface are able to report metainformation about themselves, return their input and
 * output parameters, run their action using a set of input parameters, retrieve the real name of images they output,
 * and return whether they have completed successfully. The input parameters returned by
 * {@link #createInputParameters()} should match up with the parameters used by the action's implementation of the MIPAV
 * scripting system. The {@link #scriptRun(ParameterTable)} method should be shared with the action's implementation of
 * the MIPAV scripting system.
 */
public interface ActionDiscovery {
    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata();

    /**
     * Returns a table listing the input parameters of this algorithm (which should match up with the scripting
     * parameters used in {@link #setGUIFromParams()}).
     * 
     * @return A parameter table listing the inputs of this algorithm.
     */
    public ParameterTable createInputParameters();

    /**
     * Returns a table listing the output parameters of this algorithm (usually just labels used to obtain output image
     * names later).
     * 
     * @return A parameter table listing the outputs of this algorithm.
     */
    public ParameterTable createOutputParameters();

    /**
     * Starts execution of the action. Should be the same as the scriptRun() method used by the MIPAV scripting system.
     * 
     * @param table The input parameters (with values set) to be used to run the action.
     */
    public void scriptRun(final ParameterTable table);

    /**
     * Returns the name of an image output by this algorithm, the image returned depends on the parameter label given
     * (which can be used to retrieve the image object from the image registry).
     * 
     * @param imageParamName The output image parameter label for which to get the image name.
     * @return The image name of the requested output image parameter label.
     */
    public String getOutputImageName(final String imageParamName);

    /**
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    public boolean isActionComplete();
}
