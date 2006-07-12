package gov.nih.mipav.model.scripting;


import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.view.dialogs.AlgorithmParameters;


/**
 * @see      gov.nih.mipav.view.dialogs.JDialogGaussianBlur
 * @see      gov.nih.mipav.view.dialogs.AlgorithmParameters
 * @author   Nathan Pollack
 * @version  1.0 June 28, 2006
 */
public class ScriptParameters extends AlgorithmParameters {
    /**
     * Creates a new ScriptParameters object to be used to record the current parameters entered into the algorithm's
     * GUI by the user.
     */
    public ScriptParameters() {
        super();
    }
    
    /**
     * Creates a new ScriptParameters object to be used to set up the algorithm's GUI from stored parameters.
     *
     * @param  parsedParams  stored parameters which should be used to set up the algorithm's variables/GUI.
     */
    public ScriptParameters(ParameterTable parsedParams) {
        super(parsedParams);
    }
 
    //~ Methods --------------------------------------------------------------------------------------------------------
    public void doPostAlgorithmActions() {}
    public void storeParamsFromGUI() throws ParserException {}
    public void setGUIFromParams() {}
}