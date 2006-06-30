package gov.nih.mipav.model.scripting;


import java.util.Hashtable;


/**
 * A table used to store variables and their values.
 */
public class VariableTable extends Hashtable {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4108175711902767830L;

    /** The reference to the only VariableTable which should ever be instantiated. */
    protected static VariableTable singletonReference = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new VariableTable object.
     */
    protected VariableTable() {
        super();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Returns a reference to the variable table singleton.
     *
     * @return  A reference to the variable table.
     */
    public static final VariableTable getReference() {

        if (singletonReference == null) {
            singletonReference = new VariableTable();
        }

        return singletonReference;
    }

    /**
     * Interpolates and returns the value of a given variable.
     *
     * @param   varName  The name of the variable to interpolate.
     *
     * @return  The variable's value.
     */
    public String interpolate(String varName) {
        return (String) super.get(varName);
    }

    /**
     * Checks to see if a variable has been added to the variable table.
     *
     * @param   varName  The name of the variable to check for.
     *
     * @return  <code>True</code> if the variable has been added to the table, <code>false</code> otherwise.
     */
    public boolean isVariableSet(String varName) {
        return super.containsKey(varName);
    }

    /**
     * Removes a variable and its value from the variable table.
     *
     * @param  varName  The name of the variable to remove.
     */
    public void removeVariable(String varName) {
        super.remove(varName);
    }

    /**
     * Stores a variable name and its value in the variable table.
     *
     * @param  varName  The name of the variable (e.g., '$save_as_file_name').
     * @param  value    The variable value.
     */
    public void storeVariable(String varName, String value) {
        super.put(varName, value);
    }
}
