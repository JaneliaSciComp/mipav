package gov.nih.mipav.model.scripting.parameters;


import gov.nih.mipav.model.scripting.ParserException;


/**
 * Factory methods for the creation of various types of Parameters.
 * 
 * @see Parameter
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
        return (ParameterBoolean) ParameterFactory.newParameter(label, Parameter.PARAM_BOOLEAN, "" + value);
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
        return (ParameterDouble) ParameterFactory.newParameter(label, Parameter.PARAM_DOUBLE, "" + value);
    }
        
    /**
     * Creates a new externally-specified image placeholder variable parameter with a given label and value (e.g., '$image1').
     *
     * @param   label  The label/name of the new parameter.
     * @param   value  The value to assign to the new parameter.
     *
     * @return  A new image placeholder variable parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     */
    protected static final ParameterExternalImage newExternalImage(String label, String value) throws ParserException {
        return (ParameterExternalImage) ParameterFactory.newParameter(label, Parameter.PARAM_EXTERNAL_IMAGE, "" + value);
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
        return (ParameterFloat) ParameterFactory.newParameter(label, Parameter.PARAM_FLOAT, "" + value);
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
        return (ParameterImage) ParameterFactory.newParameter(label, Parameter.PARAM_IMAGE, "" + value);
    }
    
    /**
     * Creates a new image placeholder variable parameter with a given label and value (e.g., '$image1').
     *
     * @param   label            The label/name of the new parameter.
     * @param   value            The value to assign to the new parameter (e.g., '$image1').
     * @param   isExternalImage  Whether the new image needs to be externally-specified (as oppossed to generated from within the script).
     *
     * @return  A new image placeholder variable parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     */
    public static final ParameterImage newImage(String label, String value, boolean isExternalImage) throws ParserException {
        if (isExternalImage) {
            // the image wasn't already stored in the variable table, therefore it wasn't the result of another script action, so it needs to be specified before the script can be run
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
        return (ParameterInt) ParameterFactory.newParameter(label, Parameter.PARAM_INT, "" + value);
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
        return (ParameterLong) ParameterFactory.newParameter(label, Parameter.PARAM_LONG, "" + value);
    }

    /**
     * Creates a new parameter with a given label and value.  This method is used to create parameters from the strings read in as part of a script line.
     *
     * @param   label  The label/name of the new parameter.
     * @param   type   A String indicating the type of the new parameter to create.
     * @param   value  The value to assign to the new parameter in String form (to be parsed according to the parameter type).
     *
     * @return  A new parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     * 
     * @see #newParameter(String, int, String)
     */
    public static final Parameter newParameter(String label, String type, String value) throws ParserException {

        if (Parameter.getTypeFromString(type) == Parameter.PARAM_LIST) {
            return new ParameterList(label, type, value);
        } else {
            return newParameter(label, Parameter.getTypeFromString(type), value);
        }
    }

 
    public static final Parameter newParameter(String label, double value) throws ParserException {
         return new ParameterDouble(label,  Parameter.PARAM_DOUBLE,  value);
    }
    public static final Parameter newParameter(String label, float value) throws ParserException {
        return new ParameterFloat(label,  Parameter.PARAM_FLOAT,  value);
   }
    public static final Parameter newParameter(String label, long value) throws ParserException {
        return new ParameterLong(label,  Parameter.PARAM_LONG,  value);
   }
    public static final Parameter newParameter(String label, int value) throws ParserException {
        return new ParameterInt(label,  Parameter.PARAM_INT,  value);
   }
    public static final Parameter newParameter(String label, short value) throws ParserException {
        return new ParameterShort(label,  Parameter.PARAM_SHORT,  value);
   }
    public static final Parameter newParameter(String label, boolean value) throws ParserException {
        return new ParameterBoolean(label,  Parameter.PARAM_BOOLEAN,  value);
   }
    public static final Parameter newParameter(String label, String value) throws ParserException {
        return new ParameterString(label,  Parameter.PARAM_STRING,  value);
   }
    
    public static final Parameter newParameter(String label, int[] values) throws ParserException {
        StringBuffer valueString = new StringBuffer();
        for (int i=0;i<values.length;i++){
            valueString.append(values[i] + ",");
        }
        
        return new ParameterList(label,  "list_int" , valueString.substring(0,valueString.length()-1));
    
    }
    
    
    
    
    public static final Parameter newParameter(String label, Object value) throws ParserException {
        Parameter param = null;
        
        if (value instanceof Double)       { param = new ParameterDouble(label,  Parameter.PARAM_DOUBLE,  value.toString());        
        }else if (value instanceof Boolean){ param = new ParameterBoolean(label, Parameter.PARAM_BOOLEAN, value.toString());
        }else if (value instanceof Float)  { param = new ParameterFloat(label,   Parameter.PARAM_FLOAT,   value.toString());
        }else if (value instanceof Integer){ param = new ParameterInt(label,     Parameter.PARAM_INT,     value.toString());
        }else if (value instanceof Long)   { param = new ParameterLong(label,    Parameter.PARAM_LONG,    value.toString());
        }else if (value instanceof String) { param = new ParameterString(label,  Parameter.PARAM_STRING,  value.toString());
        }else if (value instanceof Short)  { param = new ParameterShort(label,   Parameter.PARAM_SHORT,   value.toString());
        }

        
        System.out.println("param: " + param);
        return param;
    }

    
    
    
    /**
     * Creates a new parameter with a given label and value.  The type of parameter returned is determined by the type given.  List parameters are not supported by this method.
     *
     * @param   label  The label/name of the new parameter.
     * @param   type   The type of parameter to create (PARAM_LIST is not supported through this method, use the other <code>newParameter()</code>).
     * @param   value  The value to assign to the new parameter in String form (to be parsed according to the parameter type).
     *
     * @return  A new parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     * 
     * @see #newParameter(String, String, String)
     */
    public static final Parameter newParameter(String label, int type, String value) throws ParserException {
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
                throw new ParserException(label + ": Error in parsing of a param list: typeNum = " + type + " value = " +
                                          value);

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

            case Parameter.PARAM_VARIABLE:
                param = new ParameterVariable(label, type, value);
                break;

            default:
                throw new ParserException(label + ": Unrecognized parameter type number: " + type);
        }

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
        return (ParameterShort) ParameterFactory.newParameter(label, Parameter.PARAM_SHORT, "" + value);
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
        return (ParameterString) ParameterFactory.newParameter(label, Parameter.PARAM_STRING, "" + value);
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
        return (ParameterUShort) ParameterFactory.newParameter(label, Parameter.PARAM_USHORT, "" + value);
    }

    /**
     * Creates a new string variable parameter with a given label and value.
     *
     * @param   label  The label/name of the new parameter.
     * @param   value  The value to assign to the new parameter.
     *
     * @return  A new string variable parameter.
     *
     * @throws  ParserException  If there is a problem creating the new parameter.
     */
    public static final ParameterVariable newVariable(String label, String value) throws ParserException {
        return (ParameterVariable) ParameterFactory.newParameter(label, Parameter.PARAM_VARIABLE, "" + value);
    }
}
