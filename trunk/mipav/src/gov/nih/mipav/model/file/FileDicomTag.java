package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.util.*;


/**
 * This class holds all the important information about each DICOM tag. As the parser reads the DICOM dictionary file,
 * it stores new <code>DicomTags</code> in a standard Hashtable. Then when the DICOM file reader reads a new file, it
 * sets the value attribute of the DicomTag. The other constructor is used for private tags, because private tags do not
 * have unique attributes. Thus, the reader can read and display all the tags in a file, but may not read the data
 * properly and does not know the significance of the data. If the user wishes to know more information about the file
 * that is encoded in the private tags, then he or she needs to edit the DICOM dictionary file to include the necessary
 * private tags.
 *
 * @author  Neva Cherniavsky
 * @author  David Parsons
 * @see     FileInfoDicom
 * @see     FileDicom
 */
public class FileDicomTag extends ModelSerialCloneable {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5648244595999602634L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Integer element word (in hexadecimal). */
    private int element;

    /** Integer group word (in hexadecimal). */
    private int group;

    /** Length of the tag. */
    private int length;

    /** Pointer to more information about this tag, read in and contained within the dicom dictionary. */
    private FileDicomTagInfo tagInfo;

    /** Actual value of the tag. */
    private Object value;

    /**
     * Value representation for this tag, if the tags in this dicom file have explicit VRs. If the dicom tags have
     * implicit VRs, then the DicomDictionary VR is used.
     */
    private String valueRepresentation = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor that creates a DicomTag empty except for the group and element fields.
     *
     * @param  info  information about this tag
     */
    public FileDicomTag(FileDicomTagInfo info) {
        this.tagInfo = info;
        this.group = tagInfo.getKey().getGroupNumber();
        this.element = tagInfo.getKey().getElementNumber();
    }

    /**
     * Constructor that creates a DicomTag empty except for the name field. this is used for private tags and the name
     * field is the supposed value of the tag.
     *
     * @param  info   information about this tag
     * @param  value  the object to be stored
     */
    public FileDicomTag(FileDicomTagInfo info, Object value) {
        this.tagInfo = info;
        this.group = tagInfo.getKey().getGroupNumber();
        this.element = tagInfo.getKey().getElementNumber();

        this.value = value;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Tests whether a tag contains the same information as this tag.
     *
     * @param   obj  A object (hopefully a non-null FileDicomTag, but not required).
     *
     * @return  Whether all of the tag data (group, element, tag info, value, length) in the other tag is the same as
     *          this one. False if the tag is null or not a FileDicomTag.
     */
    public boolean equals(Object obj) {

        if ((obj != null) && (obj.getClass() == this.getClass())) {
            FileDicomTag tag = (FileDicomTag) obj;

            if ((this.group == tag.group) && (this.element == tag.element) && this.tagInfo.equals(tag.getInfo())) {
                Object thisVal = this.getValue(false);
                Object otherVal = tag.getValue(false);

                // allow for both values to be null
                if ((thisVal == null) && (otherVal == null)) {
                    return true;
                }

                if (this.length == tag.length) {

                    if (thisVal.equals(otherVal)) {
                        return true;
                    } else {

                        // might be an array of byte or short objects or something... we need to check
                        if (thisVal.getClass().isArray() && otherVal.getClass().isArray()) {
                            Object[] thisArray = (Object[]) thisVal;
                            Object[] otherArray = (Object[]) otherVal;

                            if (thisArray.length != otherArray.length) {
                                return false;
                            }

                            for (int i = 0; i < thisArray.length; i++) {

                                if (!thisArray[i].equals(otherArray[i])) {
                                    return false;
                                }
                            }

                            // same length and same values, the arrays are the same..
                            return true;
                        }
                    }
                }
            }
        }

        return false;
    }

    /**
     * Prepares this class for cleanup.
     */
    public void finalize() {
        value = null;
        //if(tagInfo !0= null) {
        	//tagInfo.finalize();
        	//tagInfo = null;
        //}
        try {
            super.finalize();
        } catch (Throwable er) {
            // cleaning up.. ignore errors
        }
    }

    /**
     * Calculates the number of bytes that the data (the object value) takes to be stored. This method returns the
     * number of data items times the sizeof the data type. This method will be so much simpler when (if) the tags are
     * seperated out as individual classes.
     *
     * @return  size of the value in bytes
     */
    public final int getDataLength() {
        int dataItems = 0;

        if (tagInfo.getType().equalsIgnoreCase("typeString")) {
            dataItems = value.toString().length();
        } else if (tagInfo.getType().equalsIgnoreCase("otherWordString")) {
            dataItems = getValueList().length;
        } else if (tagInfo.getType().equalsIgnoreCase("otherByteString")) {
            dataItems = getValueList().length;
        } else { // ????
            dataItems = getValueList().length;
        }

        return (dataItems * sizeof());
    }

    /**
     * Accessor that returns the element word of the tag (hex).
     *
     * @return  the element word
     */
    public final int getElement() {
        return element;
    }

    /**
     * Accessor that returns the group word of the tag (hex).
     *
     * @return  the group word
     */
    public final int getGroup() {
        return group;
    }

    /**
     * Return a reference to information about this tag in the dicom dictionary.
     *
     * @return  Information about this tag.
     */
    public final FileDicomTagInfo getInfo() {
        return tagInfo;
    }

    /**
     * Return the key object (group,element info) for this tag.
     *
     * @return  This tag's key.
     */
    public final FileDicomKey getKey() {
        return tagInfo.getKey();
    }

    /**
     * Return the keyword for this tag.
     *
     * @return  The tag keyword.
     */
    public final String getKeyword() {
        return tagInfo.getKeyword();
    }

    /**
     * Returns the length of this tag.
     *
     * @return  the length
     */
    public final int getLength() {
        return (getDataLength());
    }

    /**
     * Return the name of this tag.
     *
     * @return  The tag name.
     */
    public final String getName() {
        return tagInfo.getName();
    }

    /**
     * Provides the number of items held by the tag, as opposed the value specified by Value Multiplicity(VM). While a
     * VM may specify an unlimited number of values, this represents the number of values actually held. This is
     * numerically equivalent to the value of <code>getValueList().length</code>. Formerly getMultiplicity().
     *
     * @return  DOCUMENT ME!
     */
    public int getNumberOfValues() {
        int quantity = 0;

        try {

            if (tagInfo.getType().equals("typeString")) {

                // split by '\' separator chars--trying not to use java 1.4 req meth
                StringTokenizer backslash = new StringTokenizer((String) value, "\\");
                quantity = backslash.countTokens();
            } else {
                quantity = getValueList().length;
            }
        } catch (NullPointerException npe) {

            // System.out.print ("\t??--"+getKeyword()+"--\t");
            quantity = 0;
        }

        return quantity;
    }

    /**
     * Return the type of this tag (determined from its value representation (vr)).
     *
     * @return  The tag type.
     */
    public final String getType() {
        return tagInfo.getType();
    }

    /**
     * Translates the tag value into an understandable string so no other class need understand how to parse this
     * information. However, values with more than one multiple (vm > 1) are still encoded with "\" (as in unprocessable
     * DICOM tags) or " \ " as separators. A single value may then be de-coded using StringBuffer(str, "\"), and
     * ...nextToken().
     *
     * <p>Eg., The age is encoded as '014Y', so getValue() returns '14 Years'. Coded strings such as patient sex is 'F',
     * which returns 'Female'</p>
     *
     * <p>An additional note: in order to write a DICOM image correctly, the value should NOT be parsed into readable
     * form.</p>
     *
     * @param   parse  boolean indicating if we should parse (translate) the value or just return the value as-is
     *
     * @return  the value
     */
    public Object getValue(boolean parse) {
        Object returnValue;

        String vr = getValueRepresentation();
        String keyword = tagInfo.getKeyword();

        if (parse && (vr != null) && (keyword != null) && (value != null)) {

            if (vr.equals("AE")) {
                returnValue = ((String) value);
            } else if (vr.equals("AS")) {
                returnValue = fromAStoVisibleString();
            } else if (vr.equals("DA")) {
                returnValue = fromDAtoVisibleString();
            } else if (vr.equals("TM")) {
                returnValue = fromTMtoVisibleString();
            } else if (keyword.equals("PatientSex")) {
                returnValue = fromPatientSexToVisibleString();
            } else if (keyword.equals("PatientOrientation")) {
                returnValue = fromPatientOrientationToVisibleString();
            } else {
                returnValue = value;
            }
        } else {
            returnValue = value;
        }

        return returnValue;
    }

    /**
     * Returns the value(s) as an array so that each tag with a value multiplicity of more than 1 -- ie, TypeString
     * items separated by '\' -- is its own element in the array. This method will be so much simpler when (if) the tags
     * are seperated out as individual classes.
     *
     * @return  DOCUMENT ME!
     */
    public Object[] getValueList() {
        Object[] stuff = new Object[1];

        try {
            String type = tagInfo.getType();

            if (type.equals("typeString")) {

                // split by '\' separator chars--
                // so we do not use java 1.4 req meth:
                StringTokenizer backslash = new StringTokenizer((String) value, "\\");
                int quantity = backslash.countTokens();
                stuff = new Object[quantity];

                for (int i = 0; i < quantity; i++) {
                    stuff[i] = backslash.nextElement();
                }
            } else if (type.equals("typeFloat")) {

                // cast items into array, if they can't be, use as array of one.
                try {
                    stuff = (Float[]) value;
                } catch (ClassCastException cce) {
                    stuff[0] = value;
                }
            } else if (type.equals("typeShort")) {

                // cast items into array, if they can't be, use as array of one.
                try {
                    stuff = (Short[]) value;
                } catch (ClassCastException cce) {
                    stuff[0] = value;
                }
            } else if (type.equals("typeInt")) {

                // cast items into array, if they can't be, use as array of one.
                try {
                    stuff = (Integer[]) value;
                } catch (ClassCastException cce) {
                    stuff[0] = value;
                }
            } else if (type.equals("typeDouble")) {

                // cast items into array, if they can't be, use as array of one.
                try {
                    stuff = (Double[]) value;
                } catch (ClassCastException cce) {
                    stuff[0] = value;
                }
            } else if (type.equals("otherByteString")) {

                // cast items into array, if they can't be, use as array of one.
                try {

                    if (value instanceof Byte[]) {
                        stuff = (Byte[]) value;
                    }
                } catch (ClassCastException cce) {
                    stuff[0] = value;
                }
            } else if (type.equals("otherWordString")) {

                // cast items into array, if they can't be, use as array of one.
                try {

                    if (value instanceof Short[]) {
                        stuff = (Short[]) value;
                    }

                    if (value instanceof String) {
                        stuff[0] = (String) value;
                    }
                } catch (ClassCastException cce) {
                    stuff[0] = value;
                }
            } else if (type.equals("typeUnknown")) {

                // cast items into array, if they can't be, use as array of one.
                try {

                    if (value instanceof Byte[]) {
                        stuff = (Byte[]) value;
                    } else if (value instanceof Short[]) {
                        stuff = (Short[]) value;
                    } else if (value instanceof Integer[]) {
                        stuff = (Integer[]) value;
                    } else {
                        stuff = (Float[]) value;
                    }
                } catch (ClassCastException cce) {
                    stuff[0] = value;
                }
            }
        } catch (NullPointerException npe) {
            System.out.print("\tFileDicomTag.getValueList(): ??--" + tagInfo.getKeyword() +
                             "--cannot be dealt with.\n");
            Preferences.debug("\"" + tagInfo.getKeyword() + "\" cannot be found as a list;" +
                              " this may be an error.  \n");
        }

        return stuff;
    }

    /**
     * Accessor that returns the value multiplicity of the tag. The value multiplicity is how many instances of this
     * value representation (VR) there can be in one tag.
     *
     * @return  the value multiplicity
     */
    public final int getValueMultiplicity() {
        return tagInfo.getValueMultiplicity();
    }

    /**
     * Return the value representation (vr) of this tag. If the tag VR for this dicom are implicit, then the VR is
     * retrieved from the DicomDictionary.
     *
     * @return  The tag vr.
     */
    public final String getValueRepresentation() {

        // explicit VR
        if (this.valueRepresentation != null) {
            return this.valueRepresentation;
        }

        // implicit VR from the dicom dictionary
        return tagInfo.getValueRepresentation();
    }

    /**
     * Returns the unique identifier's hash code.
     *
     * @return  The hash code.
     */
    public final int hashCode() {

        // TODO: this might not be a good hash code...
        int hash = tagInfo.hashCode() + group + element + length + value.hashCode();

        if (valueRepresentation != null) {
            hash += valueRepresentation.hashCode();
        }

        return hash;
    }

    /**
     * Sets the Element of this DICOM tag as read in by FileDicom.
     *
     * @param  element  element to set to
     */
    public final void setElement(int element) {
        this.element = element;
    }


    /**
     * Sets the Group of this DICOM tag as read in by FileDicom.
     *
     * @param  group  group to set to
     */
    public final void setGroup(int group) {
        this.group = group;
    }

    /**
     * Sets the length of this DICOM tag as read in by FileDicom.
     *
     * @param  length  length to set to
     */
    public final void setLength(int length) {
        this.length = length;
    }

    /**
     * Sets the value and length attributes of the DicomTag. Parses value so the stored value is converted from a
     * standard, English-readable string to a DICOM v3 form. If the parsing routine is unavailable, the value gets
     * stored <b>as is</b>. Sending strings to set the value of tags of other types (eg., short) will attempt to convert
     * the string to the correct type. Attempting to store a value which is too large for the given type will throw an
     * exception. In any case, the proper size is used.
     *
     * <p>For tags with neither value representation nor keyword (a private tag), this method ignores the value. To add,
     * the user class <b>must</b> use setValue(). typeSequence and typeUnknown must also explicitly use setValue().</p>
     *
     * @param  value  the value to store
     */
    public void setValue(Object value) {
        String vr = getValueRepresentation();
        String keyword = tagInfo.getKeyword();

        // if the vr is null or has no keyword (maybe a private tag?), ignore.  Must call setValue
        if ((vr == null) || (keyword == null)) {
            return;
        }

        String type = tagInfo.getType();

        if (type.equals("typeString")) {
            String val;

            if (vr.equals("AE")) { // vr: Application Entity
                val = (String) value;
            } else if (vr.equals("AS")) { // vr: Age String
                val = fromVisibleStringToAS(value.toString());
            } else if (vr.equals("DA")) { // vr: Date
                val = fromVisibleStringToDA(value.toString());
            } else if (vr.equals("TM")) { // vr: Time
                val = fromVisibleStringToTM(value.toString());
            } else if (keyword.equals("PatientSex")) { // Patient Sex
                val = fromVisibleStringToPatientSex(value.toString());
            } else if (keyword.equals("PatientOrientation")) { // Patient Orientation
                val = fromVisibleStringToPatientOrientation(value.toString());
            } else {
                val = value.toString();
            }

            setValue(val, val.length());
        }
        // explicitly call setValue(obj, len) for sequences, and unknowns
        else if (type.equals("typeSequence") || type.equals("typeUnknown") || type.equals("otherWordString") ||
                     type.equals("otherByteString")) {
            return;
        }
        else {
            // all other types convert the value to an appropriate type when a string,
            // or just setValue.  ?May throw an exception?
            // if value has lower case letter l instead of number 1 substitute 1 for l
            String valS = (String)value;
            String valT = null;
            for (int i = 0; i < valS.length(); i++) {
                if ((valS.charAt(i) == 'l') && (valT == null)){
                    valT = "1";
                }
                else if (valS.charAt(i) == '1') {   
                    valT = valT.concat("1"); 
                }
                else {
                    valT = valT.concat(valS.substring(i,i));
                }
            }
            if (type.equals("typeFloat")) {
    
                if (value instanceof java.lang.String) {
                    setValue(Float.valueOf(valT), 4);
                } else {
                    setValue(value, 4);
                }
            } else if (type.equals("typeDouble")) {
    
                if (value instanceof java.lang.String) {
                    setValue(Float.valueOf(valT), 8);
                } else {
                    setValue(value, 8);
                }
            } else if (type.equals("typeShort")) {
    
                if (value instanceof java.lang.String) {
                    setValue(Short.valueOf(valT), 2);
                } else {
                    setValue(value, 2);
                }
            } else if (type.equals("typeInt")) {
    
                if (value instanceof java.lang.String) {
                    setValue(Integer.valueOf(valT), 4);
                } else {
                    setValue(value, 4);
                }
            }
        }
    }

    /**
     * Sets the value attribute of the DicomTag. Does NOT parse so stored value is the same as the one read in. getValue
     * does parse.
     *
     * <p>NOTE: DICOM compliant codes will be accepted.</p>
     *
     * @param  value   the value to store
     * @param  length  length of the tag
     */
    public final void setValue(Object value, int length) {

        // illegal on UIDs which are to be padded by null when not even length
        if (((length % 2) != 0) && (value instanceof java.lang.String)) {
            value = (String) value + " ";
            this.length = length + 1;
        } else {
            this.length = length;
        }

        this.value = value;
    }

    /**
     * Sets the value representation (vr) of this tag. This method should only be used when a dicom's tag VRs are
     * explicit. Otherwise, the DicomDictionary VR should be used (implicit).
     *
     * @param  vr  The tag's explicit value representation.
     */
    public final void setValueRepresentation(String vr) {
        this.valueRepresentation = vr;
    }

    /**
     * Gets the data size in bytes of the units held in this class. This method will be so much simpler when (if) the
     * tags are seperated out as individual classes.
     *
     * @return  DOCUMENT ME!
     */
    public int sizeof() {
        int retval = 0;
        String type = tagInfo.getType();

        if (type.equals("typeString")) {

            // loop through all values in the list to find total length of all
            // strings.... consider for other types tooo....
            retval = 1; // one char, no?
        } else if (type.equals("typeShort")) {
            retval = 2;
        } else if (type.equals("typeInt")) {
            retval = 4;
        } else if (type.equals("typeFloat")) {
            retval = 4;
        } else if (type.equals("typeDouble")) {
            retval = 8;
        } else if (type.equalsIgnoreCase("otherWordString")) {
            Object[] val = getValueList();

            if (val instanceof Short[]) {
                retval = 2;
            } else {
                retval = ((String) (val[0])).length();
            }
        } else if (type.equalsIgnoreCase("otherByteString") || type.equalsIgnoreCase("typeUnknown")) {
            Object[] val = getValueList();

            if (val instanceof Byte[]) {
                retval = 1;
            } else if (val instanceof Short[]) {
                retval = 2;
            } else if (val instanceof Integer[]) {
                retval = 4;
            } else if (val instanceof Float[]) {
                retval = 4;
            } else if (val instanceof Double[]) {
                retval = 8;
            } else {
                System.err.println("FileDicomTag.sizeof  " + val);
                retval = 1;
            }
        }

        return retval;
    }

    /**
     * Used for debugging so that the information contained in the tag can be converted to a readable form.
     *
     * @return  The tag in readable form
     */
    public String toString() {
        String s = "";
        s = "Group: " + Integer.toString(this.group, 0x10) + " Element: " + Integer.toString(this.element, 0x10);

        s += tagInfo.toString();

        if (value != null) {
            s += " Value: " + this.value;
        }

        s += " Length: " + Integer.toString(length, 0x10);

        return s;
    }

    /**
     * From age string to something that makes more sense. ages are DICOM as nnnT. That is, three digits preceeeding a
     * letter indicating the time unit; valid units are D (day), M (month), and Y (year).
     *
     * <p>This method translates the unit into the word, and drops leading zeros.</p>
     *
     * @return  DOCUMENT ME!
     */
    private String fromAStoVisibleString() {
        int multiple = 1;
        String age;
        StringBuffer output = new StringBuffer();

        // find first multiplicity
        StringTokenizer backslashTok = new StringTokenizer(value.toString(), "\\");

        while (backslashTok.hasMoreTokens() && (multiple <= getValueMultiplicity())) {
            age = backslashTok.nextToken();

            if ((age != null) && (age.length() > 3)) {

                try {

                    // get the number, but drop off any leading 0's
                    output.append(Integer.toString(Integer.parseInt(age.substring(0, 3))));
                    output.append(" ");

                    switch (age.charAt(3)) {

                        case 'D':
                        case 'd':
                            output.append("Days");
                            break;

                        case 'M':
                        case 'm':
                            output.append("Months");
                            break;

                        case 'Y':
                        case 'y':
                            output.append("Years");
                            break;
                    }
                } catch (NumberFormatException error) {
                    Preferences.debug("FileDICOMTag.fromAStoVisibleString: " + age + " \n");
                }
            }

            if (backslashTok.hasMoreTokens() && (multiple < getValueMultiplicity())) {
                output.append(" \\ ");
            }

            multiple++;
        }

        return (output.toString());
        // */
        // return value.toString();
    }

    /**
     * parses the DICOM v3 format tag to make a mm/dd/yyyy formatted date.
     *
     * @return  DOCUMENT ME!
     */
    private String fromDAtoVisibleString() {
        int multiple = 1;
        StringBuffer output = new StringBuffer();
        String date = value.toString().trim();
        String tmpStr;

        StringTokenizer backslashTok = new StringTokenizer(date, "\\");

        try {

            while (backslashTok.hasMoreTokens() && (multiple <= getValueMultiplicity())) {
                tmpStr = backslashTok.nextToken().trim();

                if (tmpStr.length() == 8) {
                    output.append(tmpStr.charAt(4)); // month
                    output.append(tmpStr.charAt(5));
                    output.append("/");
                    output.append(tmpStr.charAt(6)); // day
                    output.append(tmpStr.charAt(7));
                    output.append("/");
                    output.append(tmpStr.charAt(0)); // year
                    output.append(tmpStr.charAt(1));
                    output.append(tmpStr.charAt(2));
                    output.append(tmpStr.charAt(3));
                } else {
                    //
                }

                if (backslashTok.hasMoreTokens() && (multiple < getValueMultiplicity())) {
                    output.append(" \\ ");
                }

                multiple++;

            }
        } catch (StringIndexOutOfBoundsException error) {
            output.setLength(0);
            output.append(date);
        }


        return (output.toString());
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private String fromPatientOrientationToVisibleString() {
        String tmpValue = (String) value;
        tmpValue.trim(); // "A\V"

        String first, second;
        String s = "";
        StringTokenizer slash = new StringTokenizer(tmpValue, "\\");

        if (slash.countTokens() != 2) {
            return null;
        }

        first = slash.nextToken().trim();
        second = slash.nextToken().trim();

        if (first.equals("A")) {
            s = "Anterior";
        } else if (first.equals("P")) {
            s = "Posterior";
        } else if (first.equals("R")) {
            s = "Right";
        } else if (first.equals("L")) {
            s = "Left";
        } else if (first.equals("H")) {
            s = "Head";
        } else if (first.equals("F")) {
            s = "Foot";
        }

        s += " ";

        if (second.equals("A")) {
            s = s + "Anterior";
        } else if (second.equals("P")) {
            s = s + "Posterior";
        } else if (second.equals("R")) {
            s = s + "Right";
        } else if (second.equals("L")) {
            s = s + "Left";
        } else if (second.equals("H")) {
            s = s + "Head";
        } else if (second.equals("F")) {
            s = s + "Foot";
        }

        return s;
    }

    /**
     * Converts a DICOM patient sex tag (eg, "M") into a more understandable value (eg, "Male"). "M", "F", and "O" are
     * valid inputs. Any other value representation (VR) is converted to "Other" but an empty string (which is valid,
     * since the Patient Sex tag is Type 2, that is, required to exist, but no value is required) returns an empty
     * string.
     *
     * @return  DOCUMENT ME!
     */
    private String fromPatientSexToVisibleString() {
        String sex = new String();
        String temp = new String();
        StringTokenizer spaceTok = new StringTokenizer((String) value, " ");

        if (spaceTok.hasMoreTokens()) {
            temp = spaceTok.nextToken();

            if (temp.equals("M")) {
                sex = "Male";
            } else if (temp.equals("F")) {
                sex = "Female";
            } else {
                sex = "Other";
            }
        } else { // patient sex is type 2 tag... tag must exist, needs no value
            sex = "";
        }

        return sex;
    }

    /**
     * Takes a DICOM tag with a TM value representation and returns it in the format HH:MM:SS.frac, or as much as is
     * available to convert (ie, HH:MM could be all that is currently stored).
     *
     * @return  String HH:MM:SS.fraction
     */
    private String fromTMtoVisibleString() {
        int multiple = 1;
        String frac;
        boolean containsFrac;
        StringBuffer output = new StringBuffer();

        // find first multiplicity
        StringTokenizer backslashTok = new StringTokenizer(value.toString(), "\\");

        while (backslashTok.hasMoreTokens() && (multiple <= getValueMultiplicity())) {
            String timeEntry = backslashTok.nextToken();
            StringTokenizer colonTok = new StringTokenizer(timeEntry, ":");

            if (colonTok.countTokens() == 1) { // a no ':' is a DICOM v3.0 format

                StringTokenizer dotTok = new StringTokenizer(timeEntry, ".");

                if (dotTok.countTokens() == 2) { // there is a fractional value
                    timeEntry = dotTok.nextToken();
                    frac = dotTok.nextToken();
                    containsFrac = true;
                } else {
                    timeEntry = dotTok.nextToken();
                    frac = null;
                    containsFrac = false;
                }

                if (timeEntry.length() >= 2) { // contains at least hours
                    output.append(timeEntry.charAt(0));
                    output.append(timeEntry.charAt(1));

                    if (timeEntry.length() >= 4) { // also contains at least minutes
                        output.append(':');
                        output.append(timeEntry.charAt(2));
                        output.append(timeEntry.charAt(3));

                        if (timeEntry.length() == 6) { // also contains seconds
                            output.append(':');
                            output.append(timeEntry.charAt(4));
                            output.append(timeEntry.charAt(5));
                        }
                    }
                }

                if (containsFrac) { // if has fraction, add fractional part
                    output.append('.');
                    output.append(frac);
                }
            } else { // a pre-DICOM v3.0 version already in hh:mm:ss:frac form
                output.append(colonTok.toString()); // copy into place
            }

            if (backslashTok.hasMoreTokens() && (multiple < getValueMultiplicity())) {
                output.append(" \\ ");
            }

            multiple++;
        }

        return (output.toString());
    }

    /**
     * Converts the given readable string to a DICOM compliant age string. The following limitations apply: has to start
     * with numbers, then follow-up with the word 'days', 'months', 'years' irrespective of capitalization; a number may
     * be no larger than 3 digits, or only the top three digits are accepted.
     *
     * @param   tempValue  Original (readable) string value.
     *
     * @return  Dicom string version of age.
     */
    private String fromVisibleStringToAS(String tempValue) {
        int i;
        StringBuffer newStr = new StringBuffer(); // newStr will be the finished DICOM product
        int multiple = 1; // check if this token in the string can be valid for this.value multiplicity

        // parse out into a buffer up to the first \ (multiple values incidently),
        StringTokenizer strTokBackslash = new StringTokenizer(tempValue, "\\");

        // string has zero or more '\' in it...zero '\', but still having tokens is a vm=1:
        // (strTokBackslash.countTokens() = 1 on first pass indicates value multiplicity=1)
        // no tokens at all will be copy a null string into value (but it should not ave gone this far anyway.
        while (strTokBackslash.hasMoreTokens() && (multiple <= getValueMultiplicity())) {
            StringBuffer strBuf = new StringBuffer(); // temp holder for this multiplicity of  age string

            // parse out into a buffer the numeric partand the unit of time part
            StringTokenizer word = new StringTokenizer(strTokBackslash.nextToken(), " ");

            if (word.countTokens() > 1) { // if there is a decimal pt in there:

                String str = word.nextToken(); // parse out the number

                if (str.length() > 3) {
                    strBuf.append(str.substring(0, 3));
                } else {

                    for (i = 3; i > str.length(); i--) { // for as ahort as the string is,
                        strBuf.insert(0, '0'); // prepend 0s to make the string
                    } // 3 chars long

                    strBuf.append(str.toString());
                }

                // allowable units: Day, Week, Month, & Year.  Also allowing for plural
                str = word.nextToken();

                if (str.equalsIgnoreCase("Day") || str.equalsIgnoreCase("Days")) {
                    strBuf.append('D');
                } else if (str.equalsIgnoreCase("Week") || str.equalsIgnoreCase("Weeks")) {
                    strBuf.append('W');
                } else if (str.equalsIgnoreCase("Month") || str.equalsIgnoreCase("Months")) {
                    strBuf.append('M');
                } else if (str.equalsIgnoreCase("Year") || str.equalsIgnoreCase("Years")) {
                    strBuf.append('Y');
                } else { // if the string was something else, to keep DICOM compliant, just
                    strBuf.append('Y'); // append 'Y'ears
                }
            }

            if (strTokBackslash.hasMoreTokens() && (multiple <= getValueMultiplicity())) {
                strBuf.append('\\'); // chk multiplicity, and if there are more, add delimiter '\'
            }

            newStr.append(strBuf.toString());
            multiple++;
        }

        return newStr.toString();
    }


    /**
     * Converts the given string into a DICOM-legal value for a DA value-representation (VR) tag. places a date
     * (yyyymmdd) into value that is in either yyyymmdd (DICOM v3) or yyyy.mm.dd (< DICOM v3) or mm/dd/yyyy format.
     * Assumes that the date is reasonable okay to begin with. Saves value in DICOM v3 format as a yyyymmdd (DICOM v3).
     *
     * @param   tempValue  -- the string that represents the date. In American std mm/dd/yyyy or DIOCM's yyyymmdd or
     *                     yyyy.mm.dd
     *
     * @return  String a date in yyyymmdd DICOM v3 format
     */
    private String fromVisibleStringToDA(String tempValue) {
        int multiple = 1;
        String temp = null;
        StringBuffer date = new StringBuffer();
        StringTokenizer backslashTok = new StringTokenizer(tempValue, "\\");

        while (backslashTok.hasMoreTokens() && (multiple <= getValueMultiplicity())) {
            temp = backslashTok.nextToken();

            StringTokenizer dotTok = new StringTokenizer(temp, ".");
            StringTokenizer slashTok = new StringTokenizer(temp, "/");

            if (dotTok.countTokens() == 3) { // older version of DICOM
                date.append(dotTok.nextToken()); // year
                date.append(dotTok.nextToken()); // month
                date.append(dotTok.nextToken()); // day

            } else if (slashTok.countTokens() == 3) { // an American standard format (used in IO)

                Integer day, month, year;

                month = Integer.valueOf(slashTok.nextToken());
                day = Integer.valueOf(slashTok.nextToken());
                year = Integer.valueOf(slashTok.nextToken());

                date.append(year);

                while (date.length() < 4) { // make sure date (so far only the year)
                    date.insert(0, '0'); // has 4 chars... insert 0's for leading chars
                }

                if (date.length() > 4) {
                    date.setLength(4); // any date (only year) over 4 chars is truncated
                }

                // check that the month is a two digit number
                if (month.toString().length() == 2) {
                    date.append(month.toString());
                } else {
                    date.append('0');
                    date.append(month.toString());
                }

                // check that the day is a two digit number
                if (day.toString().length() == 2) {
                    date.append(day.toString());
                } else {
                    date.append('0');
                    date.append(day.toString());
                }
            } else if (dotTok.countTokens() == 1) { // Assume date is okay -- version 3 DICOM
                date.append(dotTok.nextToken());
            } else { // something is wrong
                date.setLength(0);
            }

            if (backslashTok.hasMoreTokens() && (multiple < getValueMultiplicity())) {
                date.append("\\");
            }

            multiple++;
        }

        return date.toString();
    }

    /**
     * Takes the input string and forms a DICOM compliant decimal string out of it. Allows multiple values (separated by
     * the '\' character.)
     *
     * @param   tempValue  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private String fromVisibleStringToDS(String tempValue) {
        int i;
        StringBuffer newStr = new StringBuffer();
        int multiple = 1; // check if this token in the string can be valid for this.value multiplicity

        // parse out into a buffer up to the first \ (multiple values incidently),
        StringTokenizer strTokBackslash = new StringTokenizer(tempValue, "\\");

        // string has zero or more '\' in it...zero '\', but still having tokens is a vm=1:
        // (strTokBackslash.countTokens() = 1 on first pass indicates value multiplicity=1)
        // no tokens at all will be copy a null string into value (but it should not ave gone this far anyway.
        while (strTokBackslash.hasMoreTokens() && (multiple <= getValueMultiplicity())) {
            StringBuffer strBuf = new StringBuffer(); // creating a new stringbuffer with each while
                                                      // because this version of java does not have
                                                      // stringBuffer.delete(int start, int end)

            // parse out into a buffer the parts seperating the decimal point
            StringTokenizer strTokPt = new StringTokenizer(strTokBackslash.nextToken(), ".");

            if (strTokPt.countTokens() > 1) { // if there is a decimal pt in there:
                strBuf.append(strTokPt.nextToken()); // parse (and add into string) everything up to decimal pt
                strBuf.append('.'); // place a decimal into string

                String str = new String(strTokPt.nextToken()); // the fractional part in str
                strBuf.append(str);
                i = 0;

                // while precision is under 6 decimal places and still under
                // DICOM limit of 16 bytes for an entry...
                while ((i < (6 - str.length())) && (strBuf.length() < 16)) {
                    strBuf.append('0'); // add a zero after the decimal
                    i++;
                }
            } else {
                strBuf.append(strTokPt.nextToken()); // string didn't represent a fractional num
            }

            if (strTokBackslash.hasMoreTokens() && (multiple <= getValueMultiplicity())) {
                strBuf.append('\\'); // if there is more DS, be sure to add the delimiter '\'
            }

            newStr.append(strBuf.toString());
            multiple++;
        }

        return newStr.toString();
    }

    /**
     * setVisibleStringToDT.
     *
     * @param   tempValue  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private String fromVisibleStringToDT(String tempValue) {
        StringBuffer dateTime = new StringBuffer();

        return dateTime.toString();
    }

    /**
     * DOCUMENT ME!
     *
     * @param   tmpValue  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private String fromVisibleStringToPatientOrientation(String tmpValue) {
        tmpValue.trim(); // "A\V"

        String first, second;
        String s = "";
        StringTokenizer spaceTok = new StringTokenizer(tmpValue);

        if (spaceTok.countTokens() != 2) {
            return null;
        }

        first = spaceTok.nextToken().trim();
        second = spaceTok.nextToken().trim();

        if (first.equals("Anterior") || first.equals("A")) {
            s = "A";
        } else if (first.equals("Posterior") || first.equals("P")) {
            s = "P";
        } else if (first.equals("Right") || first.equals("R")) {
            s = "R";
        } else if (first.equals("Left") || first.equals("L")) {
            s = "L";
        } else if (first.equals("Head") || first.equals("H")) {
            s = "H";
        } else if (first.equals("Foot") || first.equals("F")) {
            s = "F";
        }

        s += "\\";

        if (second.equals("Anterior") || second.equals("A")) {
            s = s + "A";
        } else if (second.equals("Posterior") || second.equals("P")) {
            s = s + "P";
        } else if (second.equals("Right") || second.equals("R")) {
            s = s + "R";
        } else if (second.equals("Left") || second.equals("L")) {
            s = s + "L";
        } else if (second.equals("Head") || second.equals("H")) {
            s = s + "H";
        } else if (second.equals("Foot") || second.equals("F")) {
            s = s + "F";
        }

        return s;
    }


    /**
     * Converts patient sex (eg, "Male") into a valid DICOM value (eg, "M"). "Male", "M", "Female","F", "Other" and "O"
     * are valid inputs. Any other input is converted to an empty string (which is valid, since the Patient Sex tag is
     * Type 2, that is, required to exist, but no value is required) and returned.
     *
     * @param   tempValue  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private String fromVisibleStringToPatientSex(String tempValue) {
        String sex = new String();
        String temp = new String();
        StringTokenizer spaceTok = new StringTokenizer(tempValue, " ");

        if (spaceTok.hasMoreTokens()) {
            temp = spaceTok.nextToken();

            if ((temp.equalsIgnoreCase("Male")) || (temp.equals("M"))) {
                sex = "M"; // male
            } else if ((temp.equalsIgnoreCase("Female")) || (temp.equals("F"))) {
                sex = "F"; // female
            } else if ((temp.equalsIgnoreCase("Other")) || (temp.equals("O"))) {
                sex = "O"; // other
            } else {
                sex = ""; // sex is a type 2 tag but not required
            }
        }

        return sex;
    }

    /**
     * Converts a DICOM person's name (&quot;PN&quot;) tag into a more understandable value. Used so the '^'
     * word-separation character isn't mistakenly removed.
     *
     * @param   tmpValue  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private String fromVisibleStringToPN(String tmpValue) {
        return tmpValue.replace('^', ',');
    }

    /**
     * setVisibleStringToTM.
     *
     * @param   tempValue  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private String fromVisibleStringToTM(String tempValue) {
        StringTokenizer backslashTok = new StringTokenizer(tempValue, "\\");
        int multiple = 1;
        String frac;
        boolean containsFrac;
        StringBuffer newTime = new StringBuffer();
        String timeField = new String();

        while (backslashTok.hasMoreTokens() && (multiple <= getValueMultiplicity())) {
            String temp = backslashTok.nextToken();
            StringTokenizer dotTok = new StringTokenizer(temp, ".");

            if (dotTok.countTokens() == 2) {
                temp = dotTok.nextToken();
                frac = dotTok.nextToken();

                if (frac.length() > 6) {
                    frac = frac.substring(0, 6);
                }

                containsFrac = true;
            } else {
                frac = null;
                containsFrac = false;
            }

            int k = 0; // index offset from 0 because contains colons

            if (temp.length() >= 2) {
                timeField = temp.substring(0, 2);

                if ((Integer.parseInt(timeField.toString()) < 0) || (Integer.parseInt(timeField.toString()) >= 24)) {
                    return "";
                }

                newTime.append(timeField);

                if (temp.length() > 2) {

                    if (temp.charAt(2) == ':') {
                        k = 1;
                    }
                }

                if (temp.length() >= (4 + k)) {
                    timeField = temp.substring(2 + k, 4 + k);

                    if ((Integer.parseInt(timeField.toString()) < 0) ||
                            (Integer.parseInt(timeField.toString()) >= 60)) {
                        return newTime.toString();
                    }

                    newTime.append(timeField);

                    if (temp.length() > (4 + k)) {

                        if (temp.charAt(4 + k) == ':') {
                            k += 1;
                        }
                    }

                    if (temp.length() == (6 + k)) {
                        timeField = temp.substring(4 + k, 6 + k);

                        if ((Integer.parseInt(timeField.toString()) < 0) ||
                                (Integer.parseInt(timeField.toString()) >= 60)) {
                            return newTime.toString();
                        }

                        newTime.append(timeField);
                    }
                }
            }

            if (containsFrac) {
                newTime.append('.');
                newTime.append(frac);
            }

            if (backslashTok.hasMoreTokens() && (multiple < getValueMultiplicity())) {
                newTime.append("\\");
            }

            multiple++;
        }

        return newTime.toString();
    }
}
