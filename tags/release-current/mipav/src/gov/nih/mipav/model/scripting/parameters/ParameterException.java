package gov.nih.mipav.model.scripting.parameters;

/**
 * Exception indicating an error encounted while retrieving a script parameter (i.e., it doesn't exist or is of an unexpected type).
 */
public class ParameterException extends RuntimeException {
    /**
     * The label of the parameter which there was an error retrieving.
     */
    private String parameterLabel;
    
    /**
     * Create a new ParameterException.
     * @param  paramLabel  The parameter label requested, which then caused the error.
     * @param  message     A message describing the error.
     */
    public ParameterException(String paramLabel, String message) {
        super(message);
        
        parameterLabel = paramLabel;
    }
    
    /**
     * Change the parameter label which caused this parameter exception.  
     * @param  paramLabel  The label of the parameter which we tried to retrieve from the parameter table.
     */
    public void setParameterLabel(String paramLabel) {
        parameterLabel = paramLabel;
    }
    
    /**
     * Return the parameter label which caused this parameter exception.
     * @return  The label of the parameter which we tried to retrieve from the parameter table.
     */
    public String getParameterLabel() {
        return parameterLabel;
    }
    
    /**
     * Returns a string representing the information contained in this exception.
     * @return  A string containing info about this error.
     */
    public String toString() {
        return "Error retrieving parameter: " + parameterLabel + ": " + getMessage();
    }
}
