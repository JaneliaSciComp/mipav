package gov.nih.mipav.model.scripting;


import gov.nih.mipav.model.scripting.parameters.ParameterTable;


/**
 * This object holds data parsed from a single line in a script, including the action to be taken and the parameters to pass to that action.
 */
public class ParsedActionLine {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The String representing the action which should be taken when this line is executed. */
    private String action;

    /** A table of parameters which may/will be used when the action is performed. */
    private ParameterTable paramList;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ParsedActionLine object.
     *
     * @param  newAction     The String representing the action to be taken
     * @param  newParamList  The table of parameters which may/will be used when the action is performed
     */
    public ParsedActionLine(String newAction, ParameterTable newParamList) {
        action = newAction;
        paramList = newParamList;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Returns this parsed action and its parameters in String form.  May be used to write out the action line to a script.
     *
     * @return  this parsed action and its parameters in String form
     */
    public String convertToString() {
        return action + "(" + paramList.convertToString() + ")";
    }

    /**
     * Returns the action which should be taken when this line is executed.
     *
     * @return  The String representing the action which should be taken when this line is executed.
     */
    public String getAction() {
        return action;
    }

    /**
     * Returns the number of parameters which will be passed to this action.
     *
     * @return  The number of parameters.
     */
    public int getParameterCount() {
        return paramList.size();
    }

    /**
     * Returns the table of parameters which will be passed to this action when it is executed.
     *
     * @return  The table of parameters to be used when this action is run.
     */
    public ParameterTable getParameterTable() {
        return paramList;
    }
}
