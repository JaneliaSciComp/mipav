package gov.nih.mipav.model.scripting.parameters;


import gov.nih.mipav.model.scripting.ParserException;


/**
 * A string variable parameter used in either the recording or execution of a script action.
 */
public class ParameterVariable extends ParameterString {

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ParameterVariable object.
     *
     * @param   paramLabel        The label/name to give to this parameter.
     * @param   paramTypeString   String indicating this parameter's type.
     * @param   paramValueString  The variable parameter value.
     *
     * @throws  ParserException  If there is a problem creating the parameter.
     */
    public ParameterVariable(String paramLabel, String paramTypeString, String paramValueString) throws ParserException {
        super(paramLabel, paramTypeString, paramValueString);
    }

    /**
     * Creates a new ParameterVariable object.
     *
     * @param   paramLabel        The label/name to give to this parameter.
     * @param   paramType         The type of this parameter (should be PARAM_VARIABLE).
     * @param   paramValueString  The variable parameter value.
     *
     * @throws  ParserException  If there is a problem creating the parameter.
     */
    public ParameterVariable(String paramLabel, int paramType, String paramValueString) throws ParserException {
        super(paramLabel, paramType, paramValueString);
    }
}
