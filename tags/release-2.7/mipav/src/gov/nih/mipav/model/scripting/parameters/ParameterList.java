package gov.nih.mipav.model.scripting.parameters;


import gov.nih.mipav.model.scripting.ParserException;

import java.util.Vector;


/**
 * This is a special kind of script parameter, which contains other parameters.  All of these parameters must be of the same type and are comma separated.
 */
public class ParameterList extends Parameter {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The list of parameters. */
    private Vector list;

    /** The data type of the elements in this parameter list. */
    private int listType;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ParameterList object.
     *
     * @param   paramLabel        The label/name to give to this parameter.
     * @param   paramTypeString   The type of this parameter, in string form.  The first part should indicate that it is a list, the second part should indicate the type of the elements in the parameter list.
     * @param   paramValueString  The list of parameters in string form.
     *
     * @throws  ParserException  If there is a problem creating the parameter list.
     */
    public ParameterList(String paramLabel, String paramTypeString, String paramValueString) throws ParserException {
        super(paramLabel, paramTypeString);
        setListType(getListTypeFromString(paramTypeString));
        setValue(paramValueString);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Add a parameter to the end of the list.
     *
     * @param  elem  A parameter (should not be another list).
     */
    public void addToList(Parameter elem) {
        list.add(elem);
    }

    /**
     * Returns the parameter at a particular position in the list.
     *
     * @param   index  The index of the parameter to retrieve.
     *
     * @return  A parameter from the list.
     */
    public Parameter elementAt(int index) {
        return (Parameter) list.elementAt(index);
    }

    /**
     * Return the parameter list.
     *
     * @return  The vector containing the list elements.
     */
    public Vector getList() {
        return list;
    }

    /**
     * Return the size of the parameter list.
     *
     * @return  The number of parameters in the list.
     */
    public int getListSize() {
        return list.size();
    }

    /**
     * Return the type of parameters in this parameter list.
     *
     * @return  The type of parameters in this list.
     */
    public int getListType() {
        return listType;
    }

    /**
     * Return the full type of this parameter (e.g. 'list_int').
     *
     * @return  The full type identifier of this parameter list (identifies that this is a list and the type of the list elements).
     */
    public String getTypeString() {
        return Parameter.getTypeString(getType()) + Parameter.getTypeString(getListType());
    }

    /**
     * Returns the parameter list in String form, separated by commas (with commas in the list elements escaped with backslashes ('\')).
     *
     * @return  The parameter list in String form, suitable for writing out as part of a script.
     */
    public String getValueString() {
        String str = new String();

        for (int i = 0; i < getListSize(); i++) {

            if (i > 0) {
                str += ",";
            }

            str += ParameterList.escapeSpecialCharacters(elementAt(i).getValueString());
        }

        return str;
    }

    /**
     * Removes all of the parameters from the list.
     */
    public void removeAllFromList() {
        list.removeAllElements();
    }

    /**
     * Changes the type of the elements in the list.
     *
     * @param  paramListType  The new parameter type.
     */
    public void setListType(int paramListType) {
        listType = paramListType;
    }

    /**
     * Sets the list of parameters based on a comma delimited String containing parameter values.
     *
     * @param   paramValueString  A comma delimited String containing parameter values.
     *
     * @throws  ParserException  If there is a problem encountered parsing the list element values.
     */
    public void setValue(String paramValueString) throws ParserException {
        setValue(parseList(paramValueString, listType));
    }

    /**
     * Changes the parameter list elements.
     *
     * @param  paramValue  The new parameter list elements.
     */
    public void setValue(Vector paramValue) {
        list = paramValue;
    }

    /**
     * Escape special list characters with backslashes.  Used on the String representation of parameter elements before converting the entire list to a comma-delimited string.
     *
     * @param   str  A string to escape.
     *
     * @return  The escaped string.
     */
    private static String escapeSpecialCharacters(String str) {
        return str.replaceAll(",", "\\\\,");
    }

    /**
     * Removes backslashes escaping any special list characters.  Used to remove any comma-escaping while parsing the individual values from a string and adding them to the list.
     *
     * @param   str  The escaped string.
     *
     * @return  The un-escaped string.
     */
    private static String replaceEscapedSpecialCharacters(String str) {
        return str.replaceAll("\\\\,", ",");
    }

    /**
     * Returns the type of the parameters contained in this list, based on the full type string for the list.
     *
     * @param   paramTypeString  The full type string for the list (e.g., 'list_float')
     *
     * @return  The data type of the parameters which should be in this parameter list.
     *
     * @throws  ParserException  If there is a problem encountered determining the type of the list elements.
     */
    private int getListTypeFromString(String paramTypeString) throws ParserException {
        int index = paramTypeString.lastIndexOf("_");

        if (index == -1) {
            throw new ParserException("Error parsing list type: " + getLabel());
        }

        return Parameter.getTypeFromString(paramTypeString.substring(index + 1));
    }

    /**
     * Extracts individual parameter list elements from a comma delimited String.
     *
     * @param   listString        A comma delimited String containing parameter values.
     * @param   listContentsType  The parameter data type of the list.
     *
     * @return  A vector containing the extracted parameters.
     *
     * @throws  ParserException  If there is a problem encountered parsing the list element values.
     */
    private Vector parseList(String listString, int listContentsType) throws ParserException {
        Vector strList = new Vector();

        int lastCommaIndex = -1;

        for (int i = 0; i < listString.length(); i++) {

            if (listString.charAt(i) == ',') {

                if ((i > 0) && (listString.charAt(i - 1) == '\\')) {
                    continue;
                } else {
                    strList.add(listString.substring(lastCommaIndex + 1, i));
                    lastCommaIndex = i;
                }
            }
        }

        strList.add(listString.substring(lastCommaIndex + 1));

        Vector newList = new Vector();

        for (int i = 0; i < strList.size(); i++) {
            String str = ParameterList.replaceEscapedSpecialCharacters((String) strList.elementAt(i));

            newList.add(ParameterFactory.newParameter("" + i, listContentsType, str));
        }

        return newList;
    }
}
