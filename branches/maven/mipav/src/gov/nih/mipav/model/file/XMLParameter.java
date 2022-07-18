package gov.nih.mipav.model.file;

import java.io.Serializable;
import java.util.StringTokenizer;

/**
 * <p>
 * Title: Parameter
 * </p>
 * 
 * <p>
 * Description: Public class to store information for a parameter associated with the image (infinite parameters
 * allowed per)
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
public class XMLParameter implements Serializable {

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4475674642754924771L;

    /** Parameter date. */
    private String date;

    /** Parameter description. */
    private String description;

    /** Parameter name. */
    private String name;

    /** Parameter time. */
    private String time;

    /** Parameter value. */
    private String value;

    /** Parameter value type. */
    private String valueType;

    /**
     * Creates a new parameter by name.
     * 
     * @param name String parameter name
     */
    public XMLParameter(String name) {
        this.name = name;
    }

    /**
     * Gets the parameter's date.
     * 
     * @return String date
     */
    public String getDate() {
        return this.date;
    }

    /**
     * Gets the parameter's description.
     * 
     * @return String description
     */
    public String getDescription() {
        return this.description;
    }

    /**
     * Gets the parameter's name.
     * 
     * @return String parameter name
     */
    public String getName() {
        return this.name;
    }

    /**
     * Gets the parameter's time.
     * 
     * @return String time
     */
    public String getTime() {
        return this.time;
    }

    /**
     * Gets the parameter's value.
     * 
     * @return String value
     */
    public String getValue() {
        return this.value;
    }

    /**
     * Gets the parameter's value-type.
     * 
     * @return String value-type
     */
    public String getValueType() {
        return this.valueType;
    }

    /**
     * Sets the date for the parameter.
     * 
     * @param date String date
     */
    public void setDate(String date) {
        this.date = date;
    }

    /**
     * Sets the date + T + time for the parameter.
     * 
     * @param dateTime String date-time
     */
    public void setDateTime(String dateTime) {
        StringTokenizer dt = new StringTokenizer(dateTime, "T");

        if (dt.hasMoreElements()) {
            date = dt.nextToken();
            time = dt.nextToken();
        }
    }

    /**
     * Sets the description for the parameter.
     * 
     * @param description String description
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * Sets the time for the parameter.
     * 
     * @param time String time
     */
    public void setTime(String time) {
        this.time = time;
    }

    /**
     * Sets the value for the parameter.
     * 
     * @param value String value
     */
    public void setValue(String value) {
        this.value = value;
    }

    /**
     * Sets the value type for the parameter.
     * 
     * @param valueType String value type
     */
    public void setValueType(String valueType) {
        this.valueType = valueType;
    }

    /**
     * String representation of parameter.
     * 
     * @return String string representation
     */
    public String toString() {
        String p = new String("<Parameters>");

        p += "<Parameter-name>" + getName() + "</Parameter-name>";
        p += "<Parameter-description>" + getDescription() + "</Parameter-description>";
        p += "<Value-type>" + getValueType() + "</Value-type>";
        p += "<Value>" + getValue() + "</Value>";
        p += "<Parameter-date-time>" + getDate() + "T" + getTime() + "</Parameter-date-time>";
        p += "</Parameters>";

        return p;
    }
}
