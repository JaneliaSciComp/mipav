package gov.nih.mipav.model.scripting.parameters;


import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.Preferences;

import java.util.*;
import java.util.Map.Entry;


/**
 * A lookup table containing Parameters keyed by a label/name. Can be filled by the parser with Parameters for a script
 * action execution, or filled by a scriptable dialog and used to record the parameters in a script.
 */
public class ParameterTable {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** The parameter table. The keys are parameter labels/names, the values are various Parameter subclasses. */
    Map<String, Parameter> paramTable = new LinkedHashMap<String, Parameter>();

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Removes all parameters from the table.
     */
    public void clear() {
        paramTable.clear();
    }

    /**
     * Checks to see if there is an entry in the parameter table for a particular label. It also checks to see if a
     * value for the parameter has been set in the global VariableTable.
     * 
     * @param paramLabel The label/name of the variable parameter to check for.
     * 
     * @return <code>True</code> if there is an entry in the table with the given label, <code>false</code>
     *         otherwise.
     */
    public boolean containsParameter(final String paramLabel) {
        return paramTable.containsKey(paramLabel) || VariableTable.getReference().isVariableSet(paramLabel);
    }

    /**
     * Converts all of the parameters in the table into a comma delimited list of parameters, suitable for inclusion in
     * a script. No command line overriding is performed.
     * 
     * @return The information on the parameters in the table in string form.
     */
    public String convertToString() {
        String str = new String();

        final Parameter[] params = getParameters();

        for (int i = 0; i < params.length; i++) {

            if (i > 0) {
                str += ", ";
            }

            str += params[i].convertToString();
        }

        return str;
    }

    public String convertToDPString(final ImageVariableTable table) {
        String str = new String();

        final Parameter[] params = getParameters();

        for (int i = 0; i < params.length; i++) {

            if (i > 0) {
                str += ", ";
            }
            if (params[i] instanceof ParameterImage) {
                str += table.getImageName( ((ParameterImage) params[i]).getValue());
            } else {
                str += params[i].convertToString();
            }
        }

        return str;
    }

    /**
     * Return the boolean value of one of the parameters from the table.
     * 
     * @param paramLabel The label/name of the boolean parameter to retrieve.
     * 
     * @return The requested parameter's value.
     * 
     * @throws ParameterException DOCUMENT ME!
     */
    public boolean getBoolean(final String paramLabel) {

        try {
            final Parameter param = getWithOverride(paramLabel, Parameter.PARAM_BOOLEAN);

            return ((ParameterBoolean) param).getValue();
        } catch (final NullPointerException npe) {
            throw new ParameterException(paramLabel, npe.getLocalizedMessage());
        } catch (final ClassCastException cce) {
            throw new ParameterException(paramLabel, cce.getLocalizedMessage());
        }
    }

    /**
     * Return the double value of one of the parameters from the table.
     * 
     * @param paramLabel The label/name of the double parameter to retrieve.
     * 
     * @return The requested parameter's value.
     * 
     * @throws ParameterException DOCUMENT ME!
     */
    public double getDouble(final String paramLabel) {

        try {
            final Parameter param = getWithOverride(paramLabel, Parameter.PARAM_DOUBLE);

            return ((ParameterDouble) param).getValue();
        } catch (final NullPointerException npe) {
            throw new ParameterException(paramLabel, npe.getLocalizedMessage());
        } catch (final ClassCastException cce) {
            throw new ParameterException(paramLabel, cce.getLocalizedMessage());
        }
    }

    /**
     * Return the file value of one of the parameters from the table.
     * 
     * @param paramLabel The label/name of the double parameter to retrieve.
     * 
     * @return The requested parameter's value.
     * 
     * @throws ParameterException DOCUMENT ME!
     */
    public String getFile(final String paramLabel) {

        try {
            final Parameter param = getWithOverride(paramLabel, Parameter.PARAM_FILE);

            return ((ParameterFile) param).getValue();
        } catch (final NullPointerException npe) {
            throw new ParameterException(paramLabel, npe.getLocalizedMessage());
        } catch (final ClassCastException cce) {
            throw new ParameterException(paramLabel, cce.getLocalizedMessage());
        }
    }

    /**
     * Return the float value of one of the parameters from the table.
     * 
     * @param paramLabel The label/name of the float parameter to retrieve.
     * 
     * @return The requested parameter's value.
     * 
     * @throws ParameterException DOCUMENT ME!
     */
    public float getFloat(final String paramLabel) {

        try {
            final Parameter param = getWithOverride(paramLabel, Parameter.PARAM_FLOAT);

            return ((ParameterFloat) param).getValue();
        } catch (final NullPointerException npe) {
            throw new ParameterException(paramLabel, npe.getLocalizedMessage());
        } catch (final ClassCastException cce) {
            throw new ParameterException(paramLabel, cce.getLocalizedMessage());
        }
    }

    /**
     * Return the model image assigned to one of the image parameters from the table. No overriding from the
     * VariableTable is allowed.
     * 
     * @param paramLabel The label/name of the image variable parameter to retrieve.
     * 
     * @return The requested parameter's value.
     * 
     * @throws ParameterException DOCUMENT ME!
     */
    public ModelImage getImage(final String paramLabel) {

        try {
            return getImageParameter(paramLabel).getImage();
        } catch (final NullPointerException npe) {
            throw new ParameterException(paramLabel, npe.getLocalizedMessage());
        } catch (final ClassCastException cce) {
            throw new ParameterException(paramLabel, cce.getLocalizedMessage());
        }
    }

    /**
     * Return one of the image parameters from the table. No overriding from the VariableTable is allowed.
     * 
     * @param paramLabel The label/name of the image variable parameter to retrieve.
     * 
     * @return The requested parameter.
     * 
     * @throws ParameterException DOCUMENT ME!
     */
    public ParameterImage getImageParameter(final String paramLabel) {

        try {
            return (ParameterImage) paramTable.get(paramLabel);
        } catch (final NullPointerException npe) {
            throw new ParameterException(paramLabel, npe.getLocalizedMessage());
        } catch (final ClassCastException cce) {
            throw new ParameterException(paramLabel, cce.getLocalizedMessage());
        }
    }

    /**
     * Return the integer value of one of the parameters from the table.
     * 
     * @param paramLabel The label/name of the integer parameter to retrieve.
     * 
     * @return The requested parameter's value.
     * 
     * @throws ParameterException DOCUMENT ME!
     */
    public int getInt(final String paramLabel) {

        try {
            final Parameter param = getWithOverride(paramLabel, Parameter.PARAM_INT);

            return ((ParameterInt) param).getValue();
        } catch (final NullPointerException npe) {
            throw new ParameterException(paramLabel, npe.getLocalizedMessage());
        } catch (final ClassCastException cce) {
            throw new ParameterException(paramLabel, cce.getLocalizedMessage());
        }
    }

    /**
     * Return the one of the parameters from the table (cast as a ParameterList).
     * 
     * @param paramLabel The label/name of the list parameter to retrieve.
     * 
     * @return The requested parameter's value.
     * 
     * @throws ParameterException DOCUMENT ME!
     */
    public ParameterList getList(final String paramLabel) {

        try {
            final Parameter param = getWithOverride(paramLabel, Parameter.PARAM_LIST);

            return (ParameterList) param;
        } catch (final NullPointerException npe) {
            throw new ParameterException(paramLabel, npe.getLocalizedMessage());
        } catch (final ClassCastException cce) {
            throw new ParameterException(paramLabel, cce.getLocalizedMessage());
        }
    }

    /**
     * Return the long value of one of the parameters from the table.
     * 
     * @param paramLabel The label/name of the long parameter to retrieve.
     * 
     * @return The requested parameter's value.
     * 
     * @throws ParameterException DOCUMENT ME!
     */
    public long getLong(final String paramLabel) {

        try {
            final Parameter param = getWithOverride(paramLabel, Parameter.PARAM_LONG);

            return ((ParameterLong) param).getValue();
        } catch (final NullPointerException npe) {
            throw new ParameterException(paramLabel, npe.getLocalizedMessage());
        } catch (final ClassCastException cce) {
            throw new ParameterException(paramLabel, cce.getLocalizedMessage());
        }
    }

    /**
     * Return the Object value of one of the parameters from the table.
     * 
     * @param paramLabel The label/name of the long parameter to retrieve.
     * 
     * @return The requested parameter.
     * 
     * @throws ParameterException DOCUMENT ME!
     */
    public Parameter getParameter(final String paramLabel) {

        // try {
        final Parameter param = getWithOverride(paramLabel, -1);

        return param;
        // } catch (NullPointerException npe) {
        // throw new ParameterException(paramLabel, npe.getLocalizedMessage());
        // } catch (ClassCastException cce) {
        // throw new ParameterException(paramLabel, cce.getLocalizedMessage());
        // }
    }

    /**
     * Get an array containing all of the parameters in the parameter table. No command line overriding is performed.
     * 
     * @return The parameters in the table, in no particular order.
     */
    public Parameter[] getParameters() {
        final Parameter[] paramArray = new Parameter[size()];
        paramTable.values().toArray(paramArray);

        return paramArray;
    }

    /**
     * Return the signed short value of one of the parameters from the table.
     * 
     * @param paramLabel The label/name of the signed short parameter to retrieve.
     * 
     * @return The requested parameter's value.
     * 
     * @throws ParameterException DOCUMENT ME!
     */
    public short getShort(final String paramLabel) {

        try {
            final Parameter param = getWithOverride(paramLabel, Parameter.PARAM_SHORT);

            return ((ParameterShort) param).getValue();
        } catch (final NullPointerException npe) {
            throw new ParameterException(paramLabel, npe.getLocalizedMessage());
        } catch (final ClassCastException cce) {
            throw new ParameterException(paramLabel, cce.getLocalizedMessage());
        }
    }

    /**
     * Return the string value of one of the parameters from the table.
     * 
     * @param paramLabel The label/name of the string parameter to retrieve.
     * 
     * @return The requested parameter's value.
     * 
     * @throws ParameterException DOCUMENT ME!
     */
    public String getString(final String paramLabel) {

        try {
            final Parameter param = getWithOverride(paramLabel, Parameter.PARAM_STRING);

            return ((ParameterString) param).getValue();
        } catch (final NullPointerException npe) {
            throw new ParameterException(paramLabel, npe.getLocalizedMessage());
        } catch (final ClassCastException cce) {
            throw new ParameterException(paramLabel, cce.getLocalizedMessage());
        }
    }

    /**
     * Return the unsigned short value of one of the parameters from the table.
     * 
     * @param paramLabel The label/name of the unsigned short parameter to retrieve.
     * 
     * @return The requested parameter's value.
     * 
     * @throws ParameterException DOCUMENT ME!
     */
    public short getUShort(final String paramLabel) {

        try {
            final Parameter param = getWithOverride(paramLabel, Parameter.PARAM_USHORT);

            return ((ParameterUShort) param).getValue();
        } catch (final NullPointerException npe) {
            throw new ParameterException(paramLabel, npe.getLocalizedMessage());
        } catch (final ClassCastException cce) {
            throw new ParameterException(paramLabel, cce.getLocalizedMessage());
        }
    }

    /**
     * Add a parameter to a table.
     * 
     * @param param The parameter to add.
     */
    public void put(final Parameter param) {
        put(param.getLabel(), param);
    }

    /**
     * Add a parameter to the table.
     * 
     * @param paramLabel The label/name of the parameter to add (does not have to be <code>param.getLabel()</code>.
     * @param param The parameter to add.
     */
    public void put(final String paramLabel, final Parameter param) {
        paramTable.put(paramLabel, param);
    }

    /**
     * Remove a parameter from the table.
     * 
     * @param paramLabel The label/name of the parameter to remove.
     */
    public void remove(final String paramLabel) {
        paramTable.remove(paramLabel);
    }

    /**
     * Returns the number of parameters contained in the table.
     * 
     * @return The number of parameters in the table.
     */
    public int size() {
        return paramTable.size();
    }

    /**
     * Returns a parameter from the table, possibly overriding it with a value stored in the global VariableTable.
     * 
     * @param paramLabel The label of the parameter to return.
     * @param paramType The type of the parameter we want returned, or -1 to use type from table.
     * 
     * @return The requested parameter from the table.
     * 
     * @throws ParameterException DOCUMENT ME!
     */
    protected Parameter getWithOverride(final String paramLabel, int paramType) {
        Parameter param = paramTable.get(paramLabel);

        if (paramType == -1) {
            if (param == null) {
                return null;
            }
            paramType = param.getType();
        }

        if ( ! (param instanceof ParameterImage)) {
            final VariableTable varTable = VariableTable.getReference();

            if (varTable.isVariableSet(paramLabel)) {

                try {
                    final String overrideValue = varTable.interpolate(paramLabel);
                    Preferences.debug("param table:\t Overriding parameter (" + paramLabel + ") with value:\t"
                            + overrideValue + "\n", Preferences.DEBUG_SCRIPTING);
                    param = ParameterFactory.newNonListParameter(paramLabel, paramType, overrideValue);
                } catch (final ParserException pe) {
                    throw new ParameterException(paramLabel, "Overriding of parameter value failed: "
                            + pe.getLocalizedMessage());
                }
            }
        }

        return param;
    }
    
    /**
     * Returns the set of parameter names.
     */
    public Set<String> keySet() {
    	return paramTable.keySet();
    }
    
    /**
     * Returns the set of parameter entries.
     */
    public Set<Entry<String,Parameter>> entrySet() {
    	return paramTable.entrySet();
    }
}
