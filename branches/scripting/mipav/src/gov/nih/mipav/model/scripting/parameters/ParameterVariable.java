package gov.nih.mipav.model.scripting.parameters;


import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.VariableTable;


/**
 * A string variable parameter used in either the recording or execution of a script action.  A value can be assigned to the variable through a switch on the command line.
 */
public class ParameterVariable extends ParameterString {

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ParameterVariable object.
     *
     * @param   paramLabel        The parameter variable's name (e.g., '$save_as_file_name').
     * @param   paramTypeString   String indicating this parameter's type.
     * @param   paramValueString  Meaningless.
     *
     * @throws  ParserException  If there is a problem creating the parameter.
     */
    public ParameterVariable(String paramLabel, String paramTypeString, String paramValueString) throws ParserException {
        super(paramLabel, paramTypeString, paramValueString);
        
        if (paramLabel.startsWith("$")) {
            throw new ParserException("Invalid parameter variable name.  Variable names must start with '$'; found: " + paramLabel);
        }
    }

    /**
     * Creates a new ParameterVariable object.
     *
     * @param   paramLabel        The parameter variable's name (e.g., '$save_as_file_name').
     * @param   paramType         The type of this parameter (should be PARAM_VARIABLE).
     * @param   paramValueString  Meaningless.
     *
     * @throws  ParserException  If there is a problem creating the parameter.
     */
    public ParameterVariable(String paramLabel, int paramType, String paramValueString) throws ParserException {
        super(paramLabel, paramType, paramValueString);
        
        if (paramLabel.startsWith("$")) {
            throw new ParserException("Invalid parameter variable name.  Variable names must start with '$'; found: " + paramLabel);
        }
    }
    
    /**
     * Returns the interpolated value assigned to this parameter variable in the variable table.
     *
     * @return  The value assigned to this parameter in the variable table.
     */
    public String getValue() {
        return VariableTable.getReference().interpolate(getLabel());
    }

    /**
     * Returns the interpolated value assigned to this parameter variable in the variable table.
     *
     * @return  The value assigned to this parameter in the variable table.
     */
    public String getValueString() {
        return "" + getValue();
    }
}
