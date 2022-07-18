package gov.nih.mipav.model.file;


import java.io.Serializable;
import java.util.Enumeration;
import java.util.Hashtable;

/**
 * <p>
 * Title: PSet
 * </p>
 * 
 * <p>
 * Description: Public class to store up to an infinite number of parameters... which will be in a Hashtable with
 * name as the key Note: there must be at least one parameter associated with each parameter set per XSD (XML
 * Schema)
 * </p>
 * 
 * <p>
 * Copyright: Copyright (c) 2004
 * </p>
 * 
 * <p>
 * Company:
 * </p>
 * 
 * @author not attributable
 * @version 1.0
 */
public class XMLPSet implements Serializable {

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1087870691598758048L;

    /** Current parameter name. */
    private String currentParameterName;

    /** Description of parameter. */
    private String description;

    /** Parameter hashtable. */
    private Hashtable<String,XMLParameter> parameterTable;

    /**
     * Create a new parameter set with the given description.
     * 
     * @param description String description of parameter set
     */
    public XMLPSet(String description) {
        this.description = description;
        parameterTable = new Hashtable<String,XMLParameter>();
    }

    /**
     * Adds a new parameter to the set.
     * 
     * @param name String name
     */
    public void addParameter(String name) {
        this.currentParameterName = name;
        parameterTable.put(name, new XMLParameter(name));
    }

    /**
     * Determines if the set contains a parameter with the given name.
     * 
     * @param key String parameter name (key)
     * 
     * @return boolean set contains parameter
     */
    public boolean containsKey(String key) {
        return parameterTable.containsKey(key);
    }

    /**
     * Returns the current parameter to be modified.
     * 
     * @return Parameter current parameter
     */
    public XMLParameter getCurrentParameter() {
        return (XMLParameter) parameterTable.get(currentParameterName);
    }

    /**
     * Get the parameter set description.
     * 
     * @return String description
     */
    public String getDescription() {
        return this.description;
    }
    
    public void setDescription(String desc) {
    	this.description = desc;
    }

    /**
     * Gets the parameter with the given name.
     * 
     * @param name DOCUMENT ME!
     * 
     * @return Parameter parameter
     */
    public XMLParameter getParameter(String name) {
        return parameterTable.get(name);
    }

    /**
     * Get an enumeration for the list of parameter names.
     * 
     * @return Enumeration enumeration for parameter name list
     */
    public Enumeration<String> getParameterKeys() {
        return parameterTable.keys();
    }

    /**
     * Gets the hashtable of parameters.
     * 
     * @return Hashtable parameter hashtable
     */
    public Hashtable<String,XMLParameter> getTable() {
        return this.parameterTable;
    }

    /**
     * Removes the parameter with the given name from the hashtable.
     * 
     * @param name String parameter name
     */
    public void removeParameter(String name) {
        parameterTable.remove(name);
    }

    /**
     * Returns a String representation of the Set.
     * 
     * @return String string representation
     */
    public String toString() {
        Enumeration<XMLParameter> e = getTable().elements();
        StringBuffer set = new StringBuffer("<Sets>");

        set.append("<Set-description>");
        set.append(getDescription());
        set.append("</Set-description>");

        while (e.hasMoreElements()) {

            try {
                XMLParameter p = e.nextElement();

                // set.append("<Parameters>");
                set.append(p.toString());
                // set.append("</Parameters>");
            } catch (ClassCastException cce) {
                System.err.println("This element was not a Parameter.");
            }
        }

        set.append("</Sets>");

        return set.toString();
    }

}
