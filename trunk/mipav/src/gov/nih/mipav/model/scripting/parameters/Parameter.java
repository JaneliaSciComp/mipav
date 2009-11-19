package gov.nih.mipav.model.scripting.parameters;


import gov.nih.mipav.model.scripting.ParserException;


/**
 * This is an abstract base class for all script action parameters. Subclasses must implement the functions
 * <code>getValueString()</code> and <code>setValue(String)</code> to get the value of the parameter in string form
 * (suitable for output in a script) and to set the parameter's value by parsing a String (which may have been read in
 * from a script).
 */
public abstract class Parameter {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /**
     * Parameter data type indicating a boolean value.
     * 
     * @see ParameterBoolean
     */
    public static final int PARAM_BOOLEAN = 0;

    /**
     * Parameter data type indicating a integer value.
     * 
     * @see ParameterInt
     */
    public static final int PARAM_INT = 1;

    /**
     * Parameter data type indicating a long value.
     * 
     * @see ParameterLong
     */
    public static final int PARAM_LONG = 2;

    /**
     * Parameter data type indicating a signed short value.
     * 
     * @see ParameterShort
     */
    public static final int PARAM_SHORT = 3;

    /**
     * Parameter data type indicating a unsigned short value.
     * 
     * @see ParameterUShort
     */
    public static final int PARAM_USHORT = 4;

    /**
     * Parameter data type indicating a float value.
     * 
     * @see ParameterFloat
     */
    public static final int PARAM_FLOAT = 5;

    /**
     * Parameter data type indicating a double value.
     * 
     * @see ParameterDouble
     */
    public static final int PARAM_DOUBLE = 6;

    /**
     * Parameter data type indicating a boolean value.
     * 
     * @see ParameterString
     */
    public static final int PARAM_STRING = 7;

    /**
     * Parameter data type indicating a list of values of a second type.
     * 
     * @see ParameterList
     */
    public static final int PARAM_LIST = 8;

    /**
     * Parameter data type indicating an image placeholder variable string value.
     * 
     * @see ParameterImage
     */
    public static final int PARAM_IMAGE = 9;

    /**
     * Parameter data type indicating an externally-specified (not script produced) image placeholder variable string
     * value.
     * 
     * @see ParameterExternalImage
     */
    public static final int PARAM_EXTERNAL_IMAGE = 10;

    /**
     * Parameter data type indicating a file variable string value.
     * 
     * @see ParameterFile
     */
    public static final int PARAM_FILE = 11;

    /** String values for all of the parameter value data types (the strings written out to the script). */
    private static final String[] PARAM_STRINGS_TABLE = {"boolean", "int", "long", "short", "ushort", "float",
            "double", "string", "list_", "image", "ext_image", "file"};

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** The label/name of the parameter. */
    private final String label;

    /** The parameter value data type. */
    private final int type;

    /**
     * A reference to another Parameter who this parameter relies on. The parent parameter's value can be tested against
     * a parentCondition field (specified by individual Parameter subclasses) to see whether this Parameter needs to be
     * assigned a value. Basically, this provides for a primitive conditional Parameter dependency tree.
     */
    private Parameter parent = null;

    /** The value to check the parent Parameter against before enabling this Parameter, in string format. */
    protected String parentValueString = null;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new Parameter object.
     * 
     * @param paramLabel The label/name of the new parameter.
     * @param paramType The type of the new parameter.
     * 
     * @throws ParserException If there is a problem creating the new Parameter.
     */
    protected Parameter(final String paramLabel, final int paramType) throws ParserException {
        label = paramLabel;
        type = paramType;

        try {
            Parameter.getTypeString(paramType);
        } catch (final ArrayIndexOutOfBoundsException oobe) {
            throw new ParserException("The type of script parameter " + paramLabel
                    + " does not appear to be supported.  Type number given: " + paramType);
        }
    }

    /**
     * Creates a new Parameter object.
     * 
     * @param paramLabel The label/name of the new parameter.
     * @param paramTypeString A String containing the data type of the parameter value.
     * 
     * @throws ParserException If there is a problem creating the new Parameter.
     */
    protected Parameter(final String paramLabel, final String paramTypeString) throws ParserException {
        label = paramLabel;
        type = Parameter.getTypeFromString(paramTypeString);

        if (type == -1) {
            throw new ParserException("The type of script parameter " + paramLabel + " not recognized.  Type given: "
                    + paramTypeString);
        }
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Returns the value of the Parameter converted into String form (suitable for writing out to a script).
     * 
     * @return A String representation of this Parameter's value (suitable for writing out to a script).
     */
    public abstract String getValueString();

    /**
     * Changes this parameter's value.
     * 
     * @param paramValueString The value to assign to this parameter, in string format. May be a parameter value read in
     *            from a script.
     * 
     * @throws ParserException If there the new value is invalid for this type of Parameter.
     */
    public abstract void setValue(String paramValueString) throws ParserException;

    /**
     * Extracts and returns the base data type (e.g., <code>PARAM_LIST</code> if 'list_float') from a type String read
     * in from a script.
     * 
     * @param typeString The type String to parse and find the base type of.
     * 
     * @return The type indicated by <code>typeString</code>.
     */
    public static final int getTypeFromString(final String typeString) {

        for (int i = 0; i < Parameter.PARAM_STRINGS_TABLE.length; i++) {

            if (typeString.toLowerCase().startsWith(Parameter.PARAM_STRINGS_TABLE[i])) {
                return i;
            }
        }

        return -1;
    }

    /**
     * Returns the String which should be used to indicate a given parameter data type when writing it to a script.
     * 
     * @param paramType The parameter type.
     * 
     * @return The String representation of the given parameter type.
     */
    public static final String getTypeString(final int paramType) {
        return Parameter.PARAM_STRINGS_TABLE[paramType];
    }

    /**
     * Retrieves the String representation of this Parameter, suitable for inclusion in a script line (e.g., &quot;label
     * type value&quot; (including the quotes)).
     * 
     * @return A String representation of this Parameter.
     */
    public String convertToString() {
        return "\"" + getLabel() + " " + getTypeString() + " " + getValueString() + "\"";
    }

    /**
     * Returns the parameter's label/name.
     * 
     * @return The parameter label.
     */
    public String getLabel() {
        return label;
    }

    /**
     * Returns the parameter data type.
     * 
     * @return The data type of the parameter's value.
     */
    public int getType() {
        return type;
    }

    /**
     * Returns the String representation of the parameter's data type.
     * 
     * @return The data type of the parameter's value in String form.
     */
    public String getTypeString() {
        return Parameter.getTypeString(type);
    }

    /**
     * Sets the reference to another Parameter who this parameter relies on. The parent parameter's value can be tested
     * against a parentCondition field (specified by individual Parameter subclasses) to see whether this Parameter
     * needs to be assigned a value.
     * 
     * @param p The parent Parameter of this Parameter.
     */
    public void setParent(final Parameter p) {
        parent = p;
    }

    /**
     * Returns a reference to another Parameter upon which this Parameter depends.
     * 
     * @return The parent Parameter.
     */
    public Parameter getParent() {
        return parent;
    }

    /**
     * Tests whether the value of the parent parameter is the same as the parent value that this Parameter requires.
     * Both the parent and the parent value string need to be set.
     * 
     * @return True if this Parameter should be displayed, false if the conditions for this parameter have not been met.
     */
    public boolean isParentConditionValueMet() {
        if (getParent() != null && getParentConditionValueString() != null) {
            return getParent().getValueString().equals(getParentConditionValueString());
        }

        return false;
    }

    /**
     * Returns the parent Parameter condition value to test against when deciding whether to ask for a value for this
     * Parameter when using the ActionDiscovery interface.
     * 
     * @return The value to check the parent Parameter against before enabling this Parameter, in string format.
     */
    public String getParentConditionValueString() {
        return parentValueString;
    }

    /**
     * Changes the parent Parameter condition value to test against when deciding whether to ask for a value for this
     * Parameter when using the ActionDiscovery interface.
     * 
     * @param s The value to check the parent Parameter against before enabling this Parameter, in string format.
     * 
     * @throws ParserException If there the new value is invalid for this type of Parameter.
     */
    public void setParentConditionValue(final String s) throws ParserException {
        // create a dummy parameter to see if it generates a ParserException
        ParameterFactory.parseParameter("test_param", getParent().getTypeString(), s);

        parentValueString = s;
    }
}
