package gov.nih.mipav.model.scripting.parameters;


import gov.nih.mipav.model.scripting.*;

import gov.nih.mipav.view.*;


/**
 * Factory methods for the creation of various types of Parameters.
 *
 * @see  Parameter
 */
public class ParameterFactory {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Creates a new boolean parameter with a given label and value.
     *
     * @param   label  The label/name of the new parameter.
     * @param   value  The value to assign to the new parameter.
     *
     * @return  A new boolean parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     */
    public static final ParameterBoolean newBoolean(String label, boolean value) throws ParserException {
        return new ParameterBoolean(label, Parameter.PARAM_BOOLEAN, value);
    }

    /**
     * Creates a new double prescision parameter with a given label and value.
     *
     * @param   label  The label/name of the new parameter.
     * @param   value  The value to assign to the new parameter.
     *
     * @return  A new double parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     */
    public static final ParameterDouble newDouble(String label, double value) throws ParserException {
        return new ParameterDouble(label, Parameter.PARAM_DOUBLE, value);
    }

    /**
     * Creates a new floating point parameter with a given label and value.
     *
     * @param   label  The label/name of the new parameter.
     * @param   value  The value to assign to the new parameter.
     *
     * @return  A new float parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     */
    public static final ParameterFloat newFloat(String label, float value) throws ParserException {
        return new ParameterFloat(label, Parameter.PARAM_FLOAT, value);
    }

    /**
     * Creates a new image placeholder variable parameter with a given label and value (e.g., '$image1').
     *
     * @param   label            The label/name of the new parameter.
     * @param   value            The value to assign to the new parameter (e.g., '$image1').
     * @param   isExternalImage  Whether the new image needs to be externally-specified (as oppossed to generated from
     *                           within the script).
     *
     * @return  A new image placeholder variable parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     */
    public static final ParameterImage newImage(String label, String value, boolean isExternalImage)
            throws ParserException {

        if (isExternalImage) {

            // the image wasn't already stored in the variable table, therefore it wasn't the result of another script
            // action, so it needs to be specified before the script can be run
            return ParameterFactory.newExternalImage(label, value);
        } else {
            return ParameterFactory.newImage(label, value);
        }
    }

    /**
     * Creates a new integer parameter with a given label and value.
     *
     * @param   label  The label/name of the new parameter.
     * @param   value  The value to assign to the new parameter.
     *
     * @return  A new integer parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     */
    public static final ParameterInt newInt(String label, int value) throws ParserException {
        return new ParameterInt(label, Parameter.PARAM_INT, value);
    }

    /**
     * Creates a new long integer parameter with a given label and value.
     *
     * @param   label  The label/name of the new parameter.
     * @param   value  The value to assign to the new parameter.
     *
     * @return  A new long parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     */
    public static final ParameterLong newLong(String label, long value) throws ParserException {
        return new ParameterLong(label, Parameter.PARAM_LONG, value);
    }

    /**
     * Creates a new parameter with a given label and value. The type of parameter returned is determined by the type
     * given. List parameters are not supported by this method.
     *
     * @param   label  The label/name of the new parameter.
     * @param   type   The type of parameter to create (PARAM_LIST is not supported through this method, use the other
     *                 <code>newParameter()</code>).
     * @param   value  The value to assign to the new parameter in String form (to be parsed according to the parameter
     *                 type).
     *
     * @return  A new parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     *
     * @see     #parseParameter(String, String, String)
     */
    public static final Parameter newNonListParameter(String label, int type, String value) throws ParserException {
        Parameter param = null;

        switch (type) {

            case Parameter.PARAM_BOOLEAN:
                param = new ParameterBoolean(label, type, value);
                break;

            case Parameter.PARAM_DOUBLE:
                param = new ParameterDouble(label, type, value);
                break;

            case Parameter.PARAM_EXTERNAL_IMAGE:
                param = new ParameterExternalImage(label, type, value);
                break;

            case Parameter.PARAM_FLOAT:
                param = new ParameterFloat(label, type, value);
                break;

            case Parameter.PARAM_IMAGE:
                param = new ParameterImage(label, type, value);
                break;

            case Parameter.PARAM_INT:
                param = new ParameterInt(label, type, value);
                break;

            case Parameter.PARAM_LIST:
                throw new ParserException(label + ": Error in parsing of a param list: typeNum = " + type +
                                          " value = " + value);

            case Parameter.PARAM_LONG:
                param = new ParameterLong(label, type, value);
                break;

            case Parameter.PARAM_STRING:
                param = new ParameterString(label, type, value);
                break;

            case Parameter.PARAM_SHORT:
                param = new ParameterShort(label, type, value);
                break;

            case Parameter.PARAM_USHORT:
                param = new ParameterUShort(label, type, value);
                break;

            default:
                throw new ParserException(label + ": Unrecognized parameter type number: " + type);
        }

        return param;
    }

    /**
     * Creates a new double prescision parameter with a given label and value.
     *
     * @param   label  The label/name of the new parameter.
     * @param   value  The value to assign to the new parameter.
     *
     * @return  A new double parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     */
    public static final Parameter newParameter(String label, double value) throws ParserException {
        return new ParameterDouble(label, Parameter.PARAM_DOUBLE, value);
    }

    /**
     * Creates a new floating point parameter with a given label and value.
     *
     * @param   label  The label/name of the new parameter.
     * @param   value  The value to assign to the new parameter.
     *
     * @return  A new float parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     */
    public static final Parameter newParameter(String label, float value) throws ParserException {
        return new ParameterFloat(label, Parameter.PARAM_FLOAT, value);
    }

    /**
     * Creates a new long integer parameter with a given label and value.
     *
     * @param   label  The label/name of the new parameter.
     * @param   value  The value to assign to the new parameter.
     *
     * @return  A new long parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     */
    public static final Parameter newParameter(String label, long value) throws ParserException {
        return new ParameterLong(label, Parameter.PARAM_LONG, value);
    }

    /**
     * Creates a new integer parameter with a given label and value.
     *
     * @param   label  The label/name of the new parameter.
     * @param   value  The value to assign to the new parameter.
     *
     * @return  A new integer parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     */
    public static final Parameter newParameter(String label, int value) throws ParserException {
        return new ParameterInt(label, Parameter.PARAM_INT, value);
    }

    /**
     * Creates a new signed short parameter with a given label and value.
     *
     * @param   label  The label/name of the new parameter.
     * @param   value  The value to assign to the new parameter.
     *
     * @return  A new short parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     */
    public static final Parameter newParameter(String label, short value) throws ParserException {
        return new ParameterShort(label, Parameter.PARAM_SHORT, value);
    }

    /**
     * Creates a new boolean parameter with a given label and value.
     *
     * @param   label  The label/name of the new parameter.
     * @param   value  The value to assign to the new parameter.
     *
     * @return  A new boolean parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     */
    public static final Parameter newParameter(String label, boolean value) throws ParserException {
        return new ParameterBoolean(label, Parameter.PARAM_BOOLEAN, value);
    }

    /**
     * Creates a new string parameter with a given label and value.
     *
     * @param   label  The label/name of the new parameter.
     * @param   value  The value to assign to the new parameter.
     *
     * @return  A new string parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     */
    public static final Parameter newParameter(String label, String value) throws ParserException {
        return new ParameterString(label, Parameter.PARAM_STRING, value);
    }

    /**
     * Creates a new ParameterList of ParameterBooleans from an array of booleans.
     *
     * @param   label   The label/name of the new parameter.
     * @param   values  The array of booleans to put into the parameter list.
     *
     * @return  A new list parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     */
    public static final Parameter newParameter(String label, boolean[] values) throws ParserException {
        ParameterList list = new ParameterList(label, Parameter.PARAM_BOOLEAN);

        for (int i = 0; i < values.length; i++) {
            list.addToList(ParameterFactory.newBoolean("" + i, values[i]));
        }

        return list;
    }

    /**
     * Creates a new ParameterList of ParameterFloats from an array of floats.
     *
     * @param   label   The label/name of the new parameter.
     * @param   values  The array of floats to put into the parameter list.
     *
     * @return  A new list parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     */
    public static final Parameter newParameter(String label, float[] values) throws ParserException {
        ParameterList list = new ParameterList(label, Parameter.PARAM_FLOAT);

        for (int i = 0; i < values.length; i++) {
            list.addToList(ParameterFactory.newFloat("" + i, values[i]));
        }

        return list;
    }

    /**
     * Creates a new ParameterList of ParameterInts from an array of ints.
     *
     * @param   label   The label/name of the new parameter.
     * @param   values  The array of ints to put into the parameter list.
     *
     * @return  A new list parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     */
    public static final Parameter newParameter(String label, int[] values) throws ParserException {
        ParameterList list = new ParameterList(label, Parameter.PARAM_INT);

        for (int i = 0; i < values.length; i++) {
            list.addToList(ParameterFactory.newInt("" + i, values[i]));
        }

        return list;
    }

    /**
     * Creates a new ParameterList of ParameterFloats from an array of doubles.
     *
     * @param   label   The label/name of the new parameter.
     * @param   values  The array of doubles to put into the parameter list.
     *
     * @return  A new list parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     */
    public static final Parameter newParameter(String label, double[] values) throws ParserException {
        ParameterList list = new ParameterList(label, Parameter.PARAM_DOUBLE);

        for (int i = 0; i < values.length; i++) {
            list.addToList(ParameterFactory.newDouble("" + i, values[i]));
        }

        return list;
    }

    /**
     * Creates a new parameter with a given label and value. The parameter type is determined by the type of the value
     * passed in.
     *
     * @param   label  The label/name of the new parameter.
     * @param   value  The value to assign to the new parameter. Should be the of the type object associated with a
     *                 primative type, or String.
     *
     * @return  A new parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     */
    public static final Parameter newParameter(String label, Object value) throws ParserException {
        Parameter param = null;

        if (value instanceof Double) {
            param = new ParameterDouble(label, Parameter.PARAM_DOUBLE, value.toString());
        } else if (value instanceof Boolean) {
            param = new ParameterBoolean(label, Parameter.PARAM_BOOLEAN, value.toString());
        } else if (value instanceof Float) {
            param = new ParameterFloat(label, Parameter.PARAM_FLOAT, value.toString());
        } else if (value instanceof Integer) {
            param = new ParameterInt(label, Parameter.PARAM_INT, value.toString());
        } else if (value instanceof Long) {
            param = new ParameterLong(label, Parameter.PARAM_LONG, value.toString());
        } else if (value instanceof String) {
            param = new ParameterString(label, Parameter.PARAM_STRING, value.toString());
        } else if (value instanceof Short) {
            param = new ParameterShort(label, Parameter.PARAM_SHORT, value.toString());
        } else {
            throw new ParserException("Unsupported value type passed into parameter creation.");
        }

        Preferences.debug("param factory:\tCreated:\t" + param + "\n", Preferences.DEBUG_SCRIPTING);

        return param;
    }

    /**
     * Creates a new signed short parameter with a given label and value.
     *
     * @param   label  The label/name of the new parameter.
     * @param   value  The value to assign to the new parameter.
     *
     * @return  A new short parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     */
    public static final ParameterShort newShort(String label, short value) throws ParserException {
        return new ParameterShort(label, Parameter.PARAM_SHORT, value);
    }

    /**
     * Creates a new string parameter with a given label and value.
     *
     * @param   label  The label/name of the new parameter.
     * @param   value  The value to assign to the new parameter.
     *
     * @return  A new string parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     */
    public static final ParameterString newString(String label, String value) throws ParserException {
        return new ParameterString(label, Parameter.PARAM_STRING, value);
    }

    /**
     * Creates a new unsigned short parameter with a given label and value.
     *
     * @param   label  The label/name of the new parameter.
     * @param   value  The value to assign to the new parameter.
     *
     * @return  A new unsigned short parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     */
    public static final ParameterUShort newUShort(String label, short value) throws ParserException {
        return new ParameterUShort(label, Parameter.PARAM_USHORT, value);
    }

    /**
     * Creates a new parameter with a given label and value. This method is used to create parameters from the strings
     * read in as part of a script line.
     *
     * @param   label  The label/name of the new parameter.
     * @param   type   A String indicating the type of the new parameter to create.
     * @param   value  The value to assign to the new parameter in String form (to be parsed according to the parameter
     *                 type).
     *
     * @return  A new parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     *
     * @see     #newNonListParameter(String, int, String)
     */
    public static final Parameter parseParameter(String label, String type, String value) throws ParserException {

        if (Parameter.getTypeFromString(type) == Parameter.PARAM_LIST) {
            return new ParameterList(label, type, value);
        } else {
            return newNonListParameter(label, Parameter.getTypeFromString(type), value);
        }
    }

    /**
     * Creates a new externally-specified image placeholder variable parameter with a given label and value (e.g.,
     * '$image1').
     *
     * @param   label  The label/name of the new parameter.
     * @param   value  The value to assign to the new parameter.
     *
     * @return  A new image placeholder variable parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     */
    protected static final ParameterExternalImage newExternalImage(String label, String value) throws ParserException {
        return new ParameterExternalImage(label, Parameter.PARAM_EXTERNAL_IMAGE, value);
    }

    /**
     * Creates a new image placeholder variable parameter with a given label and value (e.g., '$image1').
     *
     * @param   label  The label/name of the new parameter.
     * @param   value  The value to assign to the new parameter.
     *
     * @return  A new image placeholder variable parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     */
    protected static final ParameterImage newImage(String label, String value) throws ParserException {
        return new ParameterImage(label, Parameter.PARAM_IMAGE, value);
    }
}