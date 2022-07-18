package gov.nih.mipav.model.scripting.parameters;


import gov.nih.mipav.model.scripting.ParserException;


/**
 * A singed short parameter used in either the recording or execution of a script action.
 */
public class ParameterShort extends Parameter {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** The parameter's value. */
    private short value;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ParameterShort object without any value.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * 
     * @throws ParserException If there is a problem creating the parameter.
     */
    public ParameterShort(final String paramLabel) throws ParserException {
        super(paramLabel, Parameter.PARAM_SHORT);
    }

    /**
     * Creates a new ParameterShort with a new value.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * @param paramValue The new parameter value.
     * 
     * @throws ParserException If there is a problem creating the parameter.
     */
    public ParameterShort(final String paramLabel, final short paramValue) throws ParserException {
        super(paramLabel, Parameter.PARAM_SHORT);
        setValue(paramValue);
    }

    /**
     * Creates a new ParameterShort object.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * @param paramTypeString The type of this parameter, in string form.
     * @param paramValue The new parameter value.
     * 
     * @throws ParserException If there is a problem creating the parameter.
     */
    public ParameterShort(final String paramLabel, final String paramTypeString, final short paramValue)
            throws ParserException {
        super(paramLabel, paramTypeString);
        setValue(paramValue);
    }

    /**
     * Creates a new ParameterShort object.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * @param paramTypeString The type of this parameter, in string form.
     * @param paramValueString The new parameter value in string form.
     * 
     * @throws ParserException If there is a problem creating the parameter.
     */
    public ParameterShort(final String paramLabel, final String paramTypeString, final String paramValueString)
            throws ParserException {
        super(paramLabel, paramTypeString);
        setValue(paramValueString);
    }

    /**
     * Creates a new ParameterShort object.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * @param paramType The type of this parameter (should be PARAM_SHORT).
     * @param paramValue The new parameter value.
     * 
     * @throws ParserException If there is a problem creating the parameter.
     */
    public ParameterShort(final String paramLabel, final int paramType, final short paramValue) throws ParserException {
        super(paramLabel, paramType);
        setValue(paramValue);
    }

    /**
     * Creates a new ParameterShort object.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * @param paramType The type of this parameter (should be PARAM_SHORT).
     * @param paramValueString The new parameter value in string form.
     * 
     * @throws ParserException If there is a problem creating the parameter.
     */
    public ParameterShort(final String paramLabel, final int paramType, final String paramValueString)
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
    public short getValue() {
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

        try {
            setValue(Short.parseShort(paramValueString));
        } catch (final NumberFormatException nfe) {
            throw new ParserException(getLabel() + ": Invalid parameter value: " + nfe.getMessage());
        }
    }

    /**
     * Changes the parameter's current value.
     * 
     * @param paramValue The new parameter value.
     * 
     * @throws ParserException If there is a problem changing the parameter value.
     */
    public void setValue(final short paramValue) throws ParserException {
        value = paramValue;
        setValueAssigned(true);
    }
}
