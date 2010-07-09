package gov.nih.mipav.model.scripting.parameters;


import gov.nih.mipav.model.scripting.ParserException;


/**
 * A boolean parameter used in either the recording or execution of a script action.
 */
public class ParameterBoolean extends Parameter {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** The parameter's value. */
    private boolean value;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ParameterBoolean object with no value.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * 
     * @throws ParserException If there is a problem creating the parameter.
     */
    public ParameterBoolean(final String paramLabel) throws ParserException {
        super(paramLabel, Parameter.PARAM_BOOLEAN);
    }

    /**
     * Creates a new ParameterBoolean with a new value.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * @param paramValue The new parameter value.
     * 
     * @throws ParserException If there is a problem creating the parameter.
     */
    public ParameterBoolean(final String paramLabel, final boolean paramValue) throws ParserException {
        super(paramLabel, Parameter.PARAM_BOOLEAN);
        setValue(paramValue);
    }

    /**
     * Creates a new ParameterBoolean object.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * @param paramTypeString The type of this parameter, in string form.
     * @param paramValue The new parameter value.
     * 
     * @throws ParserException If there is a problem creating the parameter.
     */
    public ParameterBoolean(final String paramLabel, final String paramTypeString, final boolean paramValue)
            throws ParserException {
        super(paramLabel, paramTypeString);
        setValue(paramValue);
    }

    /**
     * Creates a new ParameterBoolean object.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * @param paramTypeString The type of this parameter, in string form.
     * @param paramValueString The new parameter value in string form.
     * 
     * @throws ParserException If there is a problem creating the parameter.
     */
    public ParameterBoolean(final String paramLabel, final String paramTypeString, final String paramValueString)
            throws ParserException {
        super(paramLabel, paramTypeString);
        setValue(paramValueString);
    }

    /**
     * Creates a new ParameterBoolean object.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * @param paramType The type of this parameter (should be PARAM_BOOLEAN).
     * @param paramValue The new parameter value.
     * 
     * @throws ParserException If there is a problem creating the parameter.
     */
    public ParameterBoolean(final String paramLabel, final int paramType, final boolean paramValue)
            throws ParserException {
        super(paramLabel, paramType);
        setValue(paramValue);
    }

    /**
     * Creates a new ParameterBoolean object.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * @param paramType The type of this parameter (should be PARAM_BOOLEAN).
     * @param paramValueString The new parameter value in string form.
     * 
     * @throws ParserException If there is a problem creating the parameter.
     */
    public ParameterBoolean(final String paramLabel, final int paramType, final String paramValueString)
            throws ParserException {
        super(paramLabel, paramType);
        setValue(paramValueString);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Returns the parameter value.
     * 
     * @return The parameter value.
     */
    public boolean getValue() {
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
     * @param paramValueString The new parameter value in String form.
     * 
     * @throws ParserException If there is a problem changing the parameter value.
     */
    public void setValue(final String paramValueString) throws ParserException {
        setValue(Boolean.parseBoolean(paramValueString));
    }

    /**
     * Changes the parameter's current value.
     * 
     * @param paramValue The new parameter value.
     * 
     * @throws ParserException If there is a problem changing the parameter value.
     */
    public void setValue(final boolean paramValue) throws ParserException {
        value = paramValue;
        setValueAssigned(true);
    }
}
