package gov.nih.mipav.model.scripting.parameters;


import gov.nih.mipav.model.scripting.ParserException;


/**
 * A string parameter used in either the recording or execution of a script action.
 */
public class ParameterString extends Parameter {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** The parameter's value. */
    protected String value;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ParameterString object without any value.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * 
     * @throws ParserException If there is a problem creating the parameter.
     */
    public ParameterString(final String paramLabel) throws ParserException {
        this(paramLabel, Parameter.PARAM_STRING);
    }

    /**
     * Creates a new ParameterString with a new value.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * @param paramValue The new parameter value.
     * 
     * @throws ParserException If there is a problem creating the parameter.
     */
    public ParameterString(final String paramLabel, final String paramValue) throws ParserException {
        super(paramLabel, Parameter.PARAM_STRING);
        setValue(paramValue);
    }

    /**
     * Creates a new ParameterString object without any value.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * @param paramType The type of this parameter (should be PARAM_STRING).
     * 
     * @throws ParserException If there is a problem creating the parameter.
     */
    public ParameterString(final String paramLabel, final int paramType) throws ParserException {
        super(paramLabel, paramType);
    }

    /**
     * Creates a new ParameterString object.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * @param paramTypeString The type of this parameter, in string form.
     * @param paramValue The new parameter value.
     * 
     * @throws ParserException If there is a problem creating the parameter.
     */
    public ParameterString(final String paramLabel, final String paramTypeString, final String paramValue)
            throws ParserException {
        super(paramLabel, paramTypeString);
        setValue(paramValue);
    }

    /**
     * Creates a new ParameterString object.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * @param paramType The type of this parameter (should be PARAM_STRING).
     * @param paramValue The new parameter value.
     * 
     * @throws ParserException If there is a problem creating the parameter.
     */
    public ParameterString(final String paramLabel, final int paramType, final String paramValue)
            throws ParserException {
        super(paramLabel, paramType);
        setValue(paramValue);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Returns the parameter value.
     * 
     * @return The parameter value.
     */
    public String getValue() {
        return value;
    }

    /**
     * Returns the parameter value as a string.
     * 
     * @return The parameter value in string form.
     */
    public String getValueString() {
        return "" + getValue();
    }

    /**
     * Changes the parameter's current value.
     * 
     * @param paramValueString The new parameter value.
     * 
     * @throws ParserException If there is a problem changing the parameter value.
     */
    public void setValue(final String paramValueString) throws ParserException {
        value = paramValueString;
        setValueAssigned(true);
    }
}
