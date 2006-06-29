package gov.nih.mipav.model.scripting.parameters;


import gov.nih.mipav.model.scripting.ParserException;


/**
 * A boolean parameter used in either the recording or execution of a script action.
 */
public class ParameterBoolean extends Parameter {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The parameter's value. */
    private boolean value;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ParameterBoolean object.
     *
     * @param   paramLabel       The label/name to give to this parameter.
     * @param   paramTypeString  The type of this parameter, in string form.
     * @param   paramValue       The new prameter value.
     *
     * @throws  ParserException  If there is a problem creating the parameter.
     */
    public ParameterBoolean(String paramLabel, String paramTypeString, boolean paramValue) throws ParserException {
        super(paramLabel, paramTypeString);
        setValue(paramValue);
    }

    /**
     * Creates a new ParameterBoolean object.
     *
     * @param   paramLabel        The label/name to give to this parameter.
     * @param   paramTypeString   The type of this parameter, in string form.
     * @param   paramValueString  The new prameter value in string form.
     *
     * @throws  ParserException  If there is a problem creating the parameter.
     */
    public ParameterBoolean(String paramLabel, String paramTypeString, String paramValueString) throws ParserException {
        super(paramLabel, paramTypeString);
        setValue(paramValueString);
    }

    /**
     * Creates a new ParameterBoolean object.
     *
     * @param   paramLabel  The label/name to give to this parameter.
     * @param   paramType   The type of this parameter (should be PARAM_DOUBLE).
     * @param   paramValue  The new prameter value.
     *
     * @throws  ParserException  If there is a problem creating the parameter.
     */
    public ParameterBoolean(String paramLabel, int paramType, boolean paramValue) throws ParserException {
        super(paramLabel, paramType);
        setValue(paramValue);
    }

    /**
     * Creates a new ParameterBoolean object.
     *
     * @param   paramLabel        The label/name to give to this parameter.
     * @param   paramType         The type of this parameter (should be PARAM_DOUBLE).
     * @param   paramValueString  The new prameter value in string form.
     *
     * @throws  ParserException  If there is a problem creating the parameter.
     */
    public ParameterBoolean(String paramLabel, int paramType, String paramValueString) throws ParserException {
        super(paramLabel, paramType);
        setValue(paramValueString);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Returns the parameter value.
     *
     * @return  The parameter value.
     */
    public boolean getValue() {
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
        // Boolean.parseBoolean() is only introduced in jvm 1.5
        //setValue(Boolean.parseBoolean(paramValueString));
        
        setValue(Boolean.valueOf(paramValueString).booleanValue());
    }

    /**
     * Changes the parameter's current value.
     *
     * @param   paramValue  The new parameter value.
     *
     * @throws  ParserException  If there is a problem changing the parameter value.
     */
    public void setValue(boolean paramValue) throws ParserException {
        value = paramValue;
    }
}
