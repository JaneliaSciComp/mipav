package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;


/**
 * This class represents a MINC atrribute element. Atrribute elements consist of a name, a type (as in byte, char, etc),
 * a number of elements, and an array of values. Often, the type is char and thus the array is an array of chars - or a
 * string. Another example is name="valid_range", type=double, nelems=2, and array[0] = 0.0, array[1] = 4095.0.
 *
 * @author  Neva Cherniavsky
 * @see     FileInfoMinc
 * @see     FileMinc
 */
public class FileMincAttElem extends ModelSerialCloneable {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4847649204791713924L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public String name;

    /** DOCUMENT ME! */
    public int nc_type;

    /** DOCUMENT ME! */
    public int nelems;

    /** DOCUMENT ME! */
    public Object[] values;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor that creates the attribute element and initializes the array to the type of the parameter and the
     * length of the parameter.
     *
     * @param  name     The name of this attribute
     * @param  nc_type  The type of this attribute, as in byte, char, etc.
     * @param  length   The length of the array holding the value of this attribute
     */
    public FileMincAttElem(String name, int nc_type, int length) {
        this.name = name;
        this.nc_type = nc_type;
        this.nelems = length;

        switch (nc_type) {

            case FileInfoMinc.NC_BYTE:
                values = new Byte[nelems];
                break;

            case FileInfoMinc.NC_CHAR:
                values = new Character[nelems];
                break;

            case FileInfoMinc.NC_SHORT:
                values = new Short[nelems];
                break;

            case FileInfoMinc.NC_INT:
                values = new Integer[nelems];
                break;

            case FileInfoMinc.NC_FLOAT:
                values = new Float[nelems];
                break;

            case FileInfoMinc.NC_DOUBLE:
                values = new Double[nelems];
                break;

            default:
                Preferences.debug("name is " + name + " type is " + nc_type + "\n", Preferences.DEBUG_FILEIO);
                MipavUtil.displayError("Invalid type in FileInfoMinc");
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Creates a new MincAttElem with the same parameters and values as this one.
     *
     * @return  The new MincAttElem.
     */
    public Object clone() {
        FileMincAttElem elem = new FileMincAttElem(this.name, this.nc_type, this.nelems);

        for (int i = 0; i < values.length; i++) {
            elem.values[i] = this.values[i];
        }

        return elem;
    }

    /**
     * Sets the attribute's value array to the value at the indicated index.
     *
     * @param  value  Value to set.
     * @param  index  Place in the array to put the value.
     */
    public void setValue(Object value, int index) {
        values[index] = value;
    }

    /**
     * Method that converts this attribute to a readable string. Used in displayAboutInfo for the image.
     *
     * @return  The string representation of this attribute.
     */
    public String toString() {
        return "Name: " + name + " Value: " + getValueString();
    }
    
    /**
     * Returns the value stored in this minc attribute as a String.
     * 
     * @return  The values in the attribute, nc_char values are joined together, all other types are separated by
     *          spaces.
     */
    public String getValueString() {
        String s = new String();

        if (nc_type == FileInfoMinc.NC_CHAR) {

            for (int i = 0; i < values.length; i++) {
                s += values[i];
            }
        } else {

            for (int i = 0; i < values.length; i++) {
                s += "" + values[i] + " ";
            }
        }

        return s.trim();
    }
}
