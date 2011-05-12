package gov.nih.mipav.model.scripting.parameters;


import gov.nih.mipav.model.scripting.ParserException;


/**
 * A file parameter used in either the recording or execution of a script action.
 */
public class ParameterFile extends ParameterString {
    /**
     * Creates a new ParameterFile object without any value.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * 
     * @throws ParserException If there is a problem creating the parameter.
     */
    public ParameterFile(final String paramLabel) throws ParserException {
        this(paramLabel, Parameter.PARAM_FILE);
    }

    /**
     * Creates a new ParameterFile with a new value.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * @param paramValue The new parameter value.
     * 
     * @throws ParserException If there is a problem creating the parameter.
     */
    public ParameterFile(final String paramLabel, final String paramValue) throws ParserException {
        super(paramLabel, Parameter.PARAM_FILE, paramValue);
    }

    /**
     * Creates a new ParameterFile object without any value.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * @param paramType The type of this parameter (should be PARAM_FILE).
     * 
     * @throws ParserException If there is a problem creating the parameter.
     */
    public ParameterFile(final String paramLabel, final int paramType) throws ParserException {
        super(paramLabel, paramType);
    }

    /**
     * Creates a new ParameterFile object.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * @param paramTypeString The type of this parameter, in string form.
     * @param paramValue The new parameter value.
     * 
     * @throws ParserException If there is a problem creating the parameter.
     */
    public ParameterFile(final String paramLabel, final String paramTypeString, final String paramValue)
            throws ParserException {
        super(paramLabel, paramTypeString, paramValue);
    }

    /**
     * Creates a new ParameterFile object.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * @param paramType The type of this parameter (should be PARAM_FILE).
     * @param paramValue The new parameter value.
     * 
     * @throws ParserException If there is a problem creating the parameter.
     */
    public ParameterFile(final String paramLabel, final int paramType, final String paramValue) throws ParserException {
        super(paramLabel, paramType, paramValue);
    }
}
