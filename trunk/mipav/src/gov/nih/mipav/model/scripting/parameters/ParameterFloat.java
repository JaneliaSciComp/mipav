package gov.nih.mipav.model.scripting.parameters;


import gov.nih.mipav.model.scripting.ParserException;


/**
 * A floating point parameter used in either the recording or execution of a script action.
 */
public class ParameterFloat extends Parameter {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The parameter's value. */
    private float value;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ParameterFloat object.
     *
     * @param   paramLabel       The label/name to give to this parameter.
     * @param   paramTypeString  The type of this parameter, in string form.
     * @param   paramValue       The new parameter value.
     *
     * @throws  ParserException  If there is a problem creating the parameter.
     */
    public ParameterFloat(String paramLabel, String paramTypeString, float paramValue) throws ParserException {
        super(paramLabel, paramTypeString);
        setValue(paramValue);
    }

    /**
     * Creates a new ParameterFloat object.
     *
     * @param   paramLabel        The label/name to give to this parameter.
     * @param   paramTypeString   The type of this parameter, in string form.
     * @param   paramValueString  The new parameter value in string form.
     *
     * @throws  ParserException  If there is a problem creating the parameter.
     */
    public ParameterFloat(String paramLabel, String paramTypeString, String paramValueString) throws ParserException {
        super(paramLabel, paramTypeString);
        setValue(paramValueString);
    }

    /**
     * Creates a new ParameterFloat object.
     *
     * @param   paramLabel  The label/name to give to this parameter.
     * @param   paramType   The type of this parameter (should be PARAM_FLOAT).
     * @param   paramValue  The new parameter value.
     *
     * @throws  ParserException  If there is a problem creating the parameter.
     */
    public ParameterFloat(String paramLabel, int paramType, float paramValue) throws ParserException {
        super(paramLabel, paramType);
        setValue(paramValue);
    }

    /**
     * Creates a new ParameterFloat object.
     *
     * @param   paramLabel        The label/name to give to this parameter.
     * @param   paramType         The type of this parameter (should be PARAM_FLOAT).
     * @param   paramValueString  The new parameter value in string form.
     *
     * @throws  ParserException  If there is a problem creating the parameter.
     */
    public ParameterFloat(String paramLabel, int paramType, String paramValueString) throws ParserException {
        super(paramLabel, paramType);
        setValue(paramValueString);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Returns the parameter value.
     *
     * @return  The parameter value.
     */
    public float getValue() {
        return value;
    }

    /**
     * Returns the parameter value as a string.
     *
     * @return  The parameter value in string form.
     */
    public String getValueString() {
        return "" + getValue();
    }

    /**
     * Changes the parameter's current value.
     *
     * @param   paramValueString  The new parameter value in String form.
     *
     * @throws  ParserException  If there is a problem changing the parameter value.
     */
    public void setValue(String paramValueString) throws ParserException {

        try {
            setValue(Float.parseFloat(paramValueString));
        } catch (NumberFormatException nfe) {
            throw new ParserException(getLabel() + ": Invalid parameter value: " + nfe.getMessage());
        }
    }

    /**
     * Changes the parameter's current value.
     *
     * @param   paramValue  The new parameter value.
     *
     * @throws  ParserException  If there is a problem changing the parameter value.
     */
    public void setValue(float paramValue) throws ParserException {
        value = paramValue;
    }
}
