package gov.nih.mipav.model.scripting.parameters;


import gov.nih.mipav.model.scripting.ParserException;

import gov.nih.mipav.view.MipavUtil;

import java.util.Vector;


/**
 * This is a special kind of script parameter, which contains other parameters. All of these parameters must be of the
 * same type and are comma separated.
 */
public class ParameterList extends Parameter {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** The list of parameters. */
    private Vector<Parameter> list;

    /** The data type of the elements in this parameter list. */
    private int listType;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ParameterList object.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * @param listContentsType The type of the parameters contained in this list.
     * 
     * @throws ParserException If there is a problem creating the parameter list.
     */
    public ParameterList(final String paramLabel, final int listContentsType) throws ParserException {
        super(paramLabel, Parameter.PARAM_LIST);
        setListType(listContentsType);
        list = new Vector<Parameter>();
    }

    /**
     * Creates a new ParameterList object.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * @param listContentsType The type of the parameters contained in this list.
     * @param paramValueString The list of parameters in string form.
     * 
     * @throws ParserException If there is a problem creating the parameter list.
     */
    public ParameterList(final String paramLabel, final int listContentsType, final String paramValueString)
            throws ParserException {
        super(paramLabel, Parameter.PARAM_LIST);
        setListType(listContentsType);
        setValue(paramValueString);
    }

    /**
     * Creates a new ParameterList object.
     * 
     * @param paramLabel The label/name to give to this parameter.
     * @param paramTypeString The type of this parameter, in string form. The first part should indicate that it is a
     *            list, the second part should indicate the type of the elements in the parameter list.
     * @param paramValueString The list of parameters in string form.
     * 
     * @throws ParserException If there is a problem creating the parameter list.
     */
    public ParameterList(final String paramLabel, final String paramTypeString, final String paramValueString)
            throws ParserException {
        super(paramLabel, paramTypeString);
        setListType(getListTypeFromString(paramTypeString));
        setValue(paramValueString);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Add a parameter to the end of the list.
     * 
     * @param elem A parameter (should not be another list).
     */
    public void addToList(final Parameter elem) {
        if (elem.getType() == listType) {
            list.add(elem);
            setValueAssigned(true);
        } else {
            MipavUtil.displayError("Error encountered:\n Attempted to add a " + Parameter.getTypeString(elem.getType())
                    + " parameter to a " + Parameter.getTypeString(listType) + " list.");
        }
    }

    /**
     * Returns the parameter at a particular position in the list.
     * 
     * @param index The index of the parameter to retrieve.
     * 
     * @return A parameter from the list.
     */
    public Parameter elementAt(final int index) {
        return list.elementAt(index);
    }

    /**
     * Returns the contents of this list as an array of booleans, if the list contains ParameterBoolean elements.
     * 
     * @return The list converted into an array of booleans.
     */
    public boolean[] getAsBooleanArray() {
        if (listType != Parameter.PARAM_BOOLEAN) {
            throw new ParameterException(getLabel(), "Tried to cast a " + Parameter.getTypeString(listType)
                    + " list as an boolean array.");
        }

        final boolean[] array = new boolean[list.size()];
        for (int i = 0; i < array.length; i++) {
            array[i] = ((ParameterBoolean) elementAt(i)).getValue();
        }
        return array;
    }

    /**
     * Returns the contents of this list as an array of floats, if the list contains ParameterFloat elements.
     * 
     * @return The list converted into an array of floats.
     */
    public float[] getAsFloatArray() {
        if (listType != Parameter.PARAM_FLOAT) {
            throw new ParameterException(getLabel(), "Tried to cast a " + Parameter.getTypeString(listType)
                    + " list as an float array.");
        }

        final float[] array = new float[list.size()];
        for (int i = 0; i < array.length; i++) {
            array[i] = ((ParameterFloat) elementAt(i)).getValue();
        }
        return array;
    }

    /**
     * Returns the contents of this list as an array of doubles, if the list contains ParameterDoubl elements.
     * 
     * @return The list converted into an array of doubles.
     */
    public double[] getAsDoubleArray() {
        if (listType != Parameter.PARAM_DOUBLE) {
            throw new ParameterException(getLabel(), "Tried to cast a " + Parameter.getTypeString(listType)
                    + " list as an double array.");
        }

        final double[] array = new double[list.size()];
        for (int i = 0; i < array.length; i++) {
            array[i] = ((ParameterDouble) elementAt(i)).getValue();
        }
        return array;
    }

    /**
     * Returns the contents of this list as an array of ints, if the list contains ParameterInt elements.
     * 
     * @return The list converted into an array of ints.
     */
    public int[] getAsIntArray() {
        if (listType != Parameter.PARAM_INT) {
            throw new ParameterException(getLabel(), "Tried to cast a " + Parameter.getTypeString(listType)
                    + " list as an int array.");
        }

        final int[] array = new int[list.size()];
        for (int i = 0; i < array.length; i++) {
            array[i] = ((ParameterInt) elementAt(i)).getValue();
        }
        return array;
    }

    /**
     * Return the parameter list.
     * 
     * @return The vector containing the list elements.
     */
    public Vector<Parameter> getList() {
        return list;
    }

    /**
     * Return the size of the parameter list.
     * 
     * @return The number of parameters in the list.
     */
    public int getListSize() {
        return list.size();
    }

    /**
     * Return the type of parameters in this parameter list.
     * 
     * @return The type of parameters in this list.
     */
    public int getListType() {
        return listType;
    }

    /**
     * Return the full type of this parameter (e.g. 'list_int').
     * 
     * @return The full type identifier of this parameter list (identifies that this is a list and the type of the list
     *         elements).
     */
    public String getTypeString() {
        return Parameter.getTypeString(getType()) + Parameter.getTypeString(getListType());
    }

    /**
     * Returns the parameter list in String form, separated by commas (with commas in the list elements escaped with
     * backslashes ('\')).
     * 
     * @return The parameter list in String form, suitable for writing out as part of a script.
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
        setValueAssigned(false);
    }

    /**
     * Changes the type of the elements in the list.
     * 
     * @param paramListType The new parameter type.
     */
    public void setListType(final int paramListType) {
        listType = paramListType;
    }

    /**
     * Sets the list of parameters based on a comma delimited String containing parameter values.
     * 
     * @param paramValueString A comma delimited String containing parameter values.
     * 
     * @throws ParserException If there is a problem encountered parsing the list element values.
     */
    public void setValue(final String paramValueString) throws ParserException {
        setValue(ParameterList.parseList(paramValueString, listType));
    }

    /**
     * Changes the parameter list elements.
     * 
     * @param paramValue The new parameter list elements.
     */
    public void setValue(final Vector<Parameter> paramValue) {
        list = paramValue;
        setValueAssigned(true);
    }

    /**
     * Escape special list characters with backslashes. Used on the String representation of parameter elements before
     * converting the entire list to a comma-delimited string.
     * 
     * @param str A string to escape.
     * 
     * @return The escaped string.
     */
    private static String escapeSpecialCharacters(final String str) {
        return str.replaceAll(",", "\\\\,");
    }

    /**
     * Removes backslashes escaping any special list characters. Used to remove any comma-escaping while parsing the
     * individual values from a string and adding them to the list.
     * 
     * @param str The escaped string.
     * 
     * @return The un-escaped string.
     */
    private static String replaceEscapedSpecialCharacters(final String str) {
        return str.replaceAll("\\\\,", ",");
    }

    /**
     * Returns the type of the parameters contained in this list, based on the full type string for the list.
     * 
     * @param paramTypeString The full type string for the list (e.g., 'list_float')
     * 
     * @return The data type of the parameters which should be in this parameter list.
     * 
     * @throws ParserException If there is a problem encountered determining the type of the list elements.
     */
    private int getListTypeFromString(final String paramTypeString) throws ParserException {
        final int index = paramTypeString.lastIndexOf("_");

        if (index == -1) {
            throw new ParserException("Error parsing list type: " + getLabel());
        }

        return Parameter.getTypeFromString(paramTypeString.substring(index + 1));
    }

    /**
     * Extracts individual parameter list elements from a comma delimited String.
     * 
     * @param listString A comma delimited String containing parameter values.
     * @param listContentsType The parameter data type of the list.
     * 
     * @return A vector containing the extracted parameters.
     * 
     * @throws ParserException If there is a problem encountered parsing the list element values.
     */
    private static final Vector<Parameter> parseList(final String listString, final int listContentsType)
            throws ParserException {
        final Vector<String> strList = new Vector<String>();

        int lastCommaIndex = -1;

        for (int i = 0; i < listString.length(); i++) {

            if (listString.charAt(i) == ',') {

                if ( (i > 0) && (listString.charAt(i - 1) == '\\')) {
                    continue;
                } else {
                    strList.add(listString.substring(lastCommaIndex + 1, i));
                    lastCommaIndex = i;
                }
            }
        }

        // only add the rest of the string if there's something there..
        final String remainingListStr = listString.substring(lastCommaIndex + 1);
        if ( !remainingListStr.equals("")) {
            strList.add(remainingListStr);
        }

        final Vector<Parameter> newList = new Vector<Parameter>();

        for (int i = 0; i < strList.size(); i++) {
            final String str = ParameterList.replaceEscapedSpecialCharacters(strList.elementAt(i));

            newList.add(ParameterFactory.newNonListParameter("" + i, listContentsType, str));
        }

        return newList;
    }
}
