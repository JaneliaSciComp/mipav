package gov.nih.mipav.model.file;


import gov.nih.mipav.model.file.FileDicomTagInfo.NumType;
import gov.nih.mipav.model.file.FileDicomTagInfo.StringType;
import gov.nih.mipav.model.file.FileDicomTagInfo.VR;
import gov.nih.mipav.model.structures.ModelSerialCloneable;

import gov.nih.mipav.view.Preferences;

import java.util.StringTokenizer;
import java.util.Vector;


/**
 * This class holds all the important information about each DICOM tag. As the parser reads the DICOM dictionary file,
 * it stores new <code>DicomTags</code> in a standard Hashtable. Then when the DICOM file reader reads a new file, it
 * sets the value attribute of the DicomTag. The other constructor is used for private tags, because private tags do not
 * have unique attributes. Thus, the reader can read and display all the tags in a file, but may not read the data
 * properly and does not know the significance of the data. If the user wishes to know more information about the file
 * that is encoded in the private tags, then he or she needs to edit the DICOM dictionary file to include the necessary
 * private tags.
 * 
 * @author Neva Cherniavsky
 * @author David Parsons
 * @see FileInfoDicom
 * @see FileDicom
 */
public class FileDicomTag extends ModelSerialCloneable implements Comparable<FileDicomTag> {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5648244595999602634L;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------
    
    /** Integer element word (in hexadecimal). */
    private int element;

    /** Integer group word (in hexadecimal). */
    private int group;

    /** Length of the tag as defined in the input image. */
    private int length;

    /** Pointer to more information about this tag, read in and contained within the dicom dictionary. */
    private final FileDicomTagInfo tagInfo;

    /** Actual value of the tag (may be an array of elements). */
    private Object value = null;

    /**
     * Value representation for this tag, if the tags in this dicom file have explicit VRs. If the dicom tags have
     * implicit VRs, then the DicomDictionary VR is used.
     */
    private VR valueRepresentation = null;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Constructor that creates a DicomTag empty except for the group and element fields.
     * 
     * @param info information about this tag
     */
    public FileDicomTag(final FileDicomTagInfo info) {
        this.tagInfo = info;
        this.group = tagInfo.getKey().getGroupNumber();
        this.element = tagInfo.getKey().getElementNumber();
    }

    /**
     * Constructor that creates a DicomTag empty except for the name field. this is used for private tags and the name
     * field is the supposed value of the tag.
     * 
     * @param info information about this tag
     * @param value the object to be stored
     */
    public FileDicomTag(final FileDicomTagInfo info, final Object value) {
        this(info);

        setValue(value);
    }
    
    

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Tests whether a tag contains the same information as this tag.
     * 
     * @param obj A object (hopefully a non-null FileDicomTag, but not required).
     * 
     * @return Whether all of the tag data (group, element, tag info, value, length) in the other tag is the same as
     *         this one. False if the tag is null or not a FileDicomTag.
     */
    public boolean equals(final Object obj) {

        if ( (obj != null) && (obj.getClass() == this.getClass())) {
            final FileDicomTag tag = (FileDicomTag) obj;

            if ( (this.group == tag.group) && (this.element == tag.element) && this.tagInfo.equals(tag.getInfo())) {
                final Object thisVal = this.getValue(false);
                final Object otherVal = tag.getValue(false);

                // allow for both values to be null
                if ( (thisVal == null) && (otherVal == null)) {
                    return true;
                } else if(thisVal == null || otherVal == null) {
                    return false;
                }
                
                
                if (this.length == tag.length) {
                    
                    
                    if (thisVal.equals(otherVal)) {
                        return true;
                    } else {

                        // might be an array of byte or short objects or something... we need to check
                        if (thisVal.getClass().isArray() && otherVal.getClass().isArray()) {
                            final Object[] thisArray = (Object[]) thisVal;
                            final Object[] otherArray = (Object[]) otherVal;

                            if (thisArray.length != otherArray.length) {
                                return false;
                            }

                            for (int i = 0; i < thisArray.length; i++) {

                                if ( !thisArray[i].equals(otherArray[i])) {
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
        // if(tagInfo !0= null) {
        // tagInfo.finalize();
        // tagInfo = null;
        // }
        try {
            super.finalize();
        } catch (final Throwable er) {
            // cleaning up.. ignore errors
        }
    }

    /**
     * Calculates the number of bytes that the data (the object value) will take to be stored. This method returns the
     * number of data items times the sizeof the data type.  This may be different from the previously stored length of
     * the tag.
     * 
     * @return size of the value in bytes
     */
    public final int getDataLength() {
        int dataItems = 0;

        Object[] obj = getValueList();
        for(int i=0; i<obj.length; i++) {
            if(obj[i] != null) {
                if(obj[i] instanceof String) {
                	dataItems += ((String)obj[i]).length();
                	if(i != obj.length-1) {
                		dataItems++; //increment for vm placeholder
                	}
                } else {
                	dataItems++;
                }
            }
        }

        return (dataItems * sizeof());
    }

    /**
     * Accessor that returns the element word of the tag (hex).
     * 
     * @return the element word
     */
    public final int getElement() {
        return element;
    }

    /**
     * Accessor that returns the group word of the tag (hex).
     * 
     * @return the group word
     */
    public final int getGroup() {
        return group;
    }

    /**
     * Return a reference to information about this tag in the dicom dictionary.
     * 
     * @return Information about this tag.
     */
    public final FileDicomTagInfo getInfo() {
        return tagInfo;
    }

    /**
     * Return the key object (group,element info) for this tag.
     * 
     * @return This tag's key.
     */
    public final FileDicomKey getKey() {
        return tagInfo.getKey();
    }

    /**
     * Return the keyword for this tag.
     * 
     * @return The tag keyword.
     */
    public final String getKeyword() {
        return tagInfo.getKeyword();
    }

    /**
     * Returns the length of this tag.
     * 
     * @return the length
     */
    public final int getLength() {
        return length;
    }

    /**
     * Return the name of this tag.
     * 
     * @return The tag name.
     */
    public final String getName() {
        return tagInfo.getName();
    }

    /**
     * Provides the number of items held by the tag, as opposed the value specified by Value Multiplicity(VM). While a
     * VM may specify an unlimited number of values, this represents the number of values actually held. This is
     * numerically equivalent to the value of <code>getValueList().length</code>. Formerly getMultiplicity().
     * 
     * @return DOCUMENT ME!
     */
    public int getNumberOfValues() {
        int quantity = 0;

        try {

            if (tagInfo.getType().equals("typeString")) {

                // split by '\' separator chars--trying not to use java 1.4 req meth
                final StringTokenizer backslash = new StringTokenizer((String) value, "\\");
                quantity = backslash.countTokens();
            } else {
            	Object[] obj = getValueList();
            	for(int i=0; i<obj.length; i++) {
            		if(obj[i] != null) {
            			quantity++;
            		}
            	}
            }
        } catch (final NullPointerException npe) {

            // System.out.print ("\t??--"+getKeyword()+"--\t");
            quantity = 0;
        }

        return quantity;
    }

    /**
     * Return the type of this tag (determined from its value representation (vr)).
     * 
     * @return The tag type.
     */
    public final VR getType() {
        return tagInfo.getType();
    }

    /**
     * Translates the tag value into an understandable string so no other class need understand how to parse this
     * information. However, values with more than one multiple (vm > 1) are still encoded with "\" (as in unprocessable
     * DICOM tags) or " \ " as separators. A single value may then be de-coded using StringBuffer(str, "\"), and
     * ...nextToken().
     * 
     * <p>
     * Eg., The age is encoded as '014Y', so getValue() returns '14 Years'. Coded strings such as patient sex is 'F',
     * which returns 'Female'
     * </p>
     * 
     * <p>
     * An additional note: in order to write a DICOM image correctly, the value should NOT be parsed into readable form.
     * </p>
     * 
     * @param parse boolean indicating if we should parse (translate) the value or just return the value as-is
     * 
     * @return the value
     */
    public Object getValue(final boolean parse) {

        final VR vr = getValueRepresentation();
        final String keyword = tagInfo.getKeyword();
        
        if (parse && vr != null && keyword != null) {
            
            String returnValue = "";
            if(value == null) {
                return returnValue;
            }
            
            switch(vr) {
            case AS:
                returnValue = fromAStoVisibleString();
                break;
            case DA:
                returnValue = fromDAtoVisibleString();
                break;
            case TM:
                returnValue = fromTMtoVisibleString();
                break;
            case SQ:
                StringBuilder sq = new StringBuilder();
                Vector<String> v = ((FileDicomSQ)value).getSequenceDisplay();
                for(int i=0; i<v.size(); i++) {
                    sq.append(v.get(i)).append("\n");
                }
                returnValue = sq.toString();
                break;
            default:
                if (value instanceof Object[]) {
                    StringBuilder bu = new StringBuilder();
                    for(int i=0; i<((Object[])value).length; i++) {
                        bu.append(((Object[])value)[i].toString()).append("\\"); //dicom uses slash to separate elements
                    }
                    returnValue = bu.toString().trim();
                } else if (keyword.equals("PatientSex")) {
                    returnValue = fromPatientSexToVisibleString();
                } else if (keyword.equals("PatientOrientation")) {
                    returnValue = fromPatientOrientationToVisibleString();
                } else {
                    returnValue = value.toString().trim();
                }
            }
            
            return returnValue;
        } 

        return value;
    }

	/**
     * Returns the value(s) as an array so that each tag with a value multiplicity of more than 1 -- ie, TypeString
     * items separated by '\' -- is its own element in the array. This method will be so much simpler when (if) the tags
     * are seperated out as individual classes.
     * 
     * @return DOCUMENT ME!
     */
    public Object[] getValueList() {
        Object[] stuff = new Object[1];

        try {
            final VR type = tagInfo.getType();

            if(value == null) {
            	stuff[0] = null;
            } else if(value instanceof Object[]) {
                return (Object[]) value;
            } else if(value instanceof FileDicomSQ) { 
                return ((FileDicomSQ) value).getSequenceDisplay().toArray();
            } else if(value instanceof FileDicomKey) { 
                stuff[0] = value;
                return stuff;
            } else if(type.getType() instanceof StringType) {
                return ((String)value).split("\\\\");
            } else {
                stuff[0] = value;
                return stuff;
            }
        } catch (final NullPointerException npe) {
            System.out
                    .print("\tFileDicomTag.getValueList(): ??--" + tagInfo.getKeyword() + "--cannot be dealt with.\n");
            Preferences.debug("\"" + tagInfo.getKeyword() + "\" cannot be found as a list;"
                    + " this may be an error.  \n", Preferences.DEBUG_FILEIO);
        }

        return stuff;
    }

    /**
     * Accessor that returns the value multiplicity of the tag. The value multiplicity is how many instances of this
     * value representation (VR) there can be in one tag.
     * 
     * @return the value multiplicity
     */
    public final int getValueMultiplicity() {
        return tagInfo.getValueMultiplicity();
    }

    /**
     * Return the value representation (vr) of this tag. If the tag VR for this dicom are implicit, then the VR is
     * retrieved from the DicomDictionary.
     * 
     * @return The tag vr.
     */
    public final VR getValueRepresentation() {

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
     * @return The hash code.
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
     * @param element element to set to
     */
    public final void setElement(final int element) {
        this.element = element;
    }

    /**
     * Sets the Group of this DICOM tag as read in by FileDicom.
     * 
     * @param group group to set to
     */
    public final void setGroup(final int group) {
        this.group = group;
    }

    /**
     * Sets the length of this DICOM tag as read in by FileDicom.
     * 
     * @param length length to set to
     */
    public final void setLength(final int length) {
        this.length = length;
    }

    /**
     * Sets the value and length attributes of the DicomTag. Parses value so the stored value is converted from a
     * standard, English-readable string to a DICOM v3 form. If the parsing routine is unavailable, the value gets
     * stored <b>as is</b>. Sending strings to set the value of tags of other types (eg., short) will attempt to
     * convert the string to the correct type. Attempting to store a value which is too large for the given type will
     * throw an exception. In any case, the proper size is used.
     * 
     * <p>
     * For tags with neither value representation nor keyword (a private tag), this method ignores the value. To add,
     * the user class <b>must</b> use setValue(). typeSequence and typeUnknown must also explicitly use setValue().
     * </p>
     * 
     * @param value the value to store
     */
    public void setValue(Object value) {
        final VR vr = getValueRepresentation();
        final String keyword = tagInfo.getKeyword();
        
        //these need to be defined elsewhere before processing of data can continue
        if (vr == null || keyword == null || value == null) {
            return;
        }

        final VR type = tagInfo.getType();

        String val;
        if(type.getType() instanceof StringType) {
            switch(type) {
            case AS:
                val = value.toString();
                break;
            case DA:
                val = fromVisibleStringToDA(value.toString());
                break;
            case TM:
                val = fromVisibleStringToTM(value.toString());
                break;
            case SQ:
                setValue(value, -1); //length of sequence cannot be determined until sequence has been populated
                return;
            case AT:
                setValue(value, 4);
                return;
            case OW:
            case OB:
            case UN:
                setValue(value, ((Object[])value).length);
                return;
            default:
                if (keyword.equals("PatientSex")) { // Patient Sex
                    val = fromVisibleStringToPatientSex(value.toString());
                } else if (keyword.equals("PatientOrientation")) { // Patient Orientation
                    val = fromVisibleStringToPatientOrientation(value.toString());
                } else {
                    val = value.toString();
                }  
            }
            
            if(val == null) {
            	val = "";
            }
            setValue(val, val.length());
        } else if (type.getType() instanceof NumType) {
            Object[] nAr = null;
            Number[] nArFinal = null;
            Number n = null;
            if(value instanceof Object[]) {
                nAr = (Object[]) value;
            } else if(value instanceof int[]) {
                nAr = new Integer[((int[]) value).length];
                for(int i=0; i<((int[])value).length; i++) {
                    nAr[i] = Integer.valueOf(((int[])value)[i]);
                }
            } else if(value instanceof short[]) {
                nAr = new Short[((short[]) value).length];
                for(int i=0; i<((short[])value).length; i++) {
                    nAr[i] = Short.valueOf(((short[])value)[i]);
                }
            } else if(value instanceof long[]) {
                nAr = new Long[((long[]) value).length];
                for(int i=0; i<((long[])value).length; i++) {
                    nAr[i] = Long.valueOf(((long[])value)[i]);
                }
            } else if(value instanceof double[]) {
                nAr = new Double[((double[]) value).length];
                for(int i=0; i<((double[])value).length; i++) {
                    nAr[i] = Double.valueOf(((double[])value)[i]);
                }
            } else if(value instanceof float[]) {
                nAr = new Float[((float[]) value).length];
                for(int i=0; i<((float[])value).length; i++) {
                    nAr[i] = Float.valueOf(((float[])value)[i]);
                }
            } else if(value instanceof byte[]) {
                nAr = new Byte[((byte[]) value).length];
                for(int i=0; i<((byte[])value).length; i++) {
                    nAr[i] = Byte.valueOf(((byte[])value)[i]);
                }
            } 
            
            switch(((NumType)type.getType())) {
            
            case SHORT:
                if(nAr != null) {
                    if(nAr instanceof Short[]) {
                        nArFinal = (Short[]) nAr;
                    } else {
                        nArFinal = new Short[nAr.length];
                        for(int i=0; i<nAr.length; i++) {
                            nArFinal[i] = Short.valueOf(nAr[i].toString());
                        }
                    }
                } else {
                    n = Short.valueOf(value.toString());
                }
                break;
                
            case LONG:
                if(nAr != null) {
                    if(nAr instanceof Integer[]) {
                        nArFinal = (Integer[]) nAr;
                    } else {
                        nArFinal = new Integer[nAr.length];
                        for(int i=0; i<nAr.length; i++) {
                            nArFinal[i] = Integer.valueOf(nAr[i].toString());
                        }
                    }
                } else {
                    n = Integer.valueOf(value.toString());
                }
                break;
                
            case FLOAT:
                if(nAr != null) {
                    if(nAr instanceof Float[]) {
                        nArFinal = (Float[]) nAr;
                    } else {
                        nArFinal = new Float[nAr.length];
                        for(int i=0; i<nAr.length; i++) {
                            nArFinal[i] = Float.valueOf(nAr[i].toString());
                        }
                    }
                } else {
                    n = Float.valueOf(value.toString());
                }
                break;
                
            case DOUBLE:
                if(nAr != null) {
                    if(nAr instanceof Double[]) {
                        nArFinal = (Double[]) nAr;
                    } else {
                        nArFinal = new Double[nAr.length];
                        for(int i=0; i<nAr.length; i++) {
                            nArFinal[i] = Double.valueOf(nAr[i].toString());
                        }
                    }
                } else {
                    n = Double.valueOf(value.toString());
                }
                break;
            }
            
            if(nArFinal != null) {
                setValue(nArFinal, nArFinal.length*((NumType)type.getType()).getNumBytes());
            } else {
                setValue(n, ((NumType)type.getType()).getNumBytes());
            }
            
        } else {
            return;   
        }
    }

    /**
     * Sets the value attribute of the DicomTag. Does NOT parse so stored value is the same as the one read in. getValue
     * does parse.
     * 
     * <p>
     * NOTE: DICOM compliant codes will be accepted.
     * </p>
     * 
     * @param value the value to store
     * @param length length of the tag
     */
    private final void setValue(Object value, final int length) {

        // illegal on UIDs which are to be padded by null when not even length
        if ( ( (length % 2) != 0) && (value instanceof java.lang.String)) {
            value = value + new String(new byte[]{0x20});
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
     * @param vr The tag's explicit value representation.
     */
    public final void setValueRepresentation(final VR vr) {
        this.valueRepresentation = vr;
    }

    /**
     * Gets the data size in bytes of the units held in this class. 
     * 
     * @return DOCUMENT ME!
     */
    public int sizeof() {
        final VR vr = tagInfo.getType();
        final DicomType type = vr.getType();
        
        if(type instanceof NumType) {
            return ((NumType) type).getNumBytes();
        } else if(vr.equals(VR.AT)){
            return 4;
        } else if(value instanceof Short[]){
            return 2;
        } else {
            return 1;
        }
    }

    /**
     * Used for debugging so that the information contained in the tag can be converted to a readable form.
     * 
     * @return The tag in readable form
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
     * <p>
     * This method translates the unit into the word, and drops leading zeros.
     * </p>
     * 
     * @return DOCUMENT ME!
     */
    private String fromAStoVisibleString() {
        int multiple = 1;
        String age;
        final StringBuffer output = new StringBuffer();

        // find first multiplicity
        final StringTokenizer backslashTok = new StringTokenizer(value.toString(), "\\");

        while (backslashTok.hasMoreTokens() && (multiple <= getValueMultiplicity())) {
            age = backslashTok.nextToken();

            if ( (age != null) && (age.length() > 3)) {

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
                } catch (final NumberFormatException error) {
                    Preferences.debug("FileDICOMTag.fromAStoVisibleString: " + age + " \n", Preferences.DEBUG_FILEIO);
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
     * @return DOCUMENT ME!
     */
    private String fromDAtoVisibleString() {
        int multiple = 1;
        final StringBuffer output = new StringBuffer();
        final String date = value.toString().trim();
        String tmpStr;

        final StringTokenizer backslashTok = new StringTokenizer(date, "\\");

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
        } catch (final StringIndexOutOfBoundsException error) {
            output.setLength(0);
            output.append(date);
        }

        return (output.toString());
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    private String fromPatientOrientationToVisibleString() {
        String tmpValue = (String) value;
        tmpValue = tmpValue.trim(); // "A\V"

        String first, second;
        String s = "";
        final StringTokenizer slash = new StringTokenizer(tmpValue, "\\");

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
     * @return DOCUMENT ME!
     */
    private String fromPatientSexToVisibleString() {
        String sex = new String();
        String temp = new String();
        final StringTokenizer spaceTok = new StringTokenizer((String) value, " ");

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
     * @return String HH:MM:SS.fraction
     */
    private String fromTMtoVisibleString() {
        int multiple = 1;
        String frac;
        boolean containsFrac;
        final StringBuffer output = new StringBuffer();

        // find first multiplicity
        final StringTokenizer backslashTok = new StringTokenizer(value.toString(), "\\");

        while (backslashTok.hasMoreTokens() && (multiple <= getValueMultiplicity())) {
            String timeEntry = backslashTok.nextToken();
            final StringTokenizer colonTok = new StringTokenizer(timeEntry, ":");

            if (colonTok.countTokens() == 1) { // a no ':' is a DICOM v3.0 format

                final StringTokenizer dotTok = new StringTokenizer(timeEntry, ".");

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
     * @param tempValue Original (readable) string value.
     * 
     * @return Dicom string version of age.
     */
    private String fromVisibleStringToAS(final String tempValue) {
        int i;
        final StringBuffer newStr = new StringBuffer(); // newStr will be the finished DICOM product
        int multiple = 1; // check if this token in the string can be valid for this.value multiplicity

        // parse out into a buffer up to the first \ (multiple values incidently),
        final StringTokenizer strTokBackslash = new StringTokenizer(tempValue, "\\");

        // string has zero or more '\' in it...zero '\', but still having tokens is a vm=1:
        // (strTokBackslash.countTokens() = 1 on first pass indicates value multiplicity=1)
        // no tokens at all will be copy a null string into value (but it should not ave gone this far anyway.
        while (strTokBackslash.hasMoreTokens() && (multiple <= getValueMultiplicity())) {
            final StringBuffer strBuf = new StringBuffer(); // temp holder for this multiplicity of age string

            // parse out into a buffer the numeric partand the unit of time part
            final StringTokenizer word = new StringTokenizer(strTokBackslash.nextToken(), " ");

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

                // allowable units: Day, Week, Month, & Year. Also allowing for plural
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
     * @param tempValue -- the string that represents the date. In American std mm/dd/yyyy or DIOCM's yyyymmdd or
     *            yyyy.mm.dd
     * 
     * @return String a date in yyyymmdd DICOM v3 format
     */
    private String fromVisibleStringToDA(final String tempValue) {
        int multiple = 1;
        String temp = null;
        final StringBuffer date = new StringBuffer();
        final StringTokenizer backslashTok = new StringTokenizer(tempValue, "\\");

        while (backslashTok.hasMoreTokens() && (multiple <= getValueMultiplicity())) {
            temp = backslashTok.nextToken();

            final StringTokenizer dotTok = new StringTokenizer(temp, ".");
            final StringTokenizer slashTok = new StringTokenizer(temp, "/");

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
     * @param tempValue DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    @SuppressWarnings("unused")
    private String fromVisibleStringToDS(final String tempValue) {
        int i;
        final StringBuffer newStr = new StringBuffer();
        int multiple = 1; // check if this token in the string can be valid for this.value multiplicity

        // parse out into a buffer up to the first \ (multiple values incidently),
        final StringTokenizer strTokBackslash = new StringTokenizer(tempValue, "\\");

        // string has zero or more '\' in it...zero '\', but still having tokens is a vm=1:
        // (strTokBackslash.countTokens() = 1 on first pass indicates value multiplicity=1)
        // no tokens at all will be copy a null string into value (but it should not ave gone this far anyway.
        while (strTokBackslash.hasMoreTokens() && (multiple <= getValueMultiplicity())) {
            final StringBuffer strBuf = new StringBuffer(); // creating a new stringbuffer with each while
            // because this version of java does not have
            // stringBuffer.delete(int start, int end)

            // parse out into a buffer the parts seperating the decimal point
            final StringTokenizer strTokPt = new StringTokenizer(strTokBackslash.nextToken(), ".");

            if (strTokPt.countTokens() > 1) { // if there is a decimal pt in there:
                strBuf.append(strTokPt.nextToken()); // parse (and add into string) everything up to decimal pt
                strBuf.append('.'); // place a decimal into string

                final String str = new String(strTokPt.nextToken()); // the fractional part in str
                strBuf.append(str);
                i = 0;

                // while precision is under 6 decimal places and still under
                // DICOM limit of 16 bytes for an entry...
                while ( (i < (6 - str.length())) && (strBuf.length() < 16)) {
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
     * @param tempValue DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    @SuppressWarnings("unused")
    private String fromVisibleStringToDT(final String tempValue) {
        final StringBuffer dateTime = new StringBuffer();

        return dateTime.toString();
    }

    /**
     * DOCUMENT ME!
     * 
     * @param tmpValue DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    private String fromVisibleStringToPatientOrientation(String tmpValue) {
        tmpValue = tmpValue.trim(); // "A\V"

        String first, second;
        String s = "";
        final StringTokenizer spaceTok = new StringTokenizer(tmpValue);

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
     * @param tempValue DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    private String fromVisibleStringToPatientSex(final String tempValue) {
        String sex = tempValue.trim().toLowerCase();
        if(sex.contains("female") || sex.contains("f")) {
        	sex = "F";
        } else if(sex.contains("male") || sex.contains("m")){
        	sex = "M";
        } else if(sex.contains("other") || sex.contains("o")){
        	sex = "O";
        } else {
        	sex = ""; //sex is type 2 but not required
        }

        return sex;
    }

    /**
     * Converts a DICOM person's name (&quot;PN&quot;) tag into a more understandable value. Used so the '^'
     * word-separation character isn't mistakenly removed.
     * 
     * @param tmpValue DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    @SuppressWarnings("unused")
    private String fromVisibleStringToPN(final String tmpValue) {
        return tmpValue.replace('^', ',');
    }

    /**
     * setVisibleStringToTM.
     * 
     * @param tempValue DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    private String fromVisibleStringToTM(final String tempValue) {
        final StringTokenizer backslashTok = new StringTokenizer(tempValue, "\\");
        int multiple = 1;
        String frac;
        boolean containsFrac;
        final StringBuffer newTime = new StringBuffer();
        String timeField = new String();

        while (backslashTok.hasMoreTokens() && (multiple <= getValueMultiplicity())) {
            String temp = backslashTok.nextToken();
            final StringTokenizer dotTok = new StringTokenizer(temp, ".");

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

                if ( (Integer.parseInt(timeField.toString()) < 0) || (Integer.parseInt(timeField.toString()) >= 24)) {
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

                    if ( (Integer.parseInt(timeField.toString()) < 0) || (Integer.parseInt(timeField.toString()) >= 60)) {
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

                        if ( (Integer.parseInt(timeField.toString()) < 0)
                                || (Integer.parseInt(timeField.toString()) >= 60)) {
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

    /**
     * Converts a little-endian byte array into an array of Integers with length equal to the tag vm.
     * 
     * @param b The byte array.
     * @param vm The tag VM.
     * @param endianess True indicates big endian, false indicates little endian.
     * @return An Integer array with length vm.
     */
    public static final Integer[] toInt(final byte[] b, final int vm, final boolean endianess) {
        int offset = 0;
        final Integer[] intArr = new Integer[vm];
        for (int i = 0; i < vm; i++, offset += 4) {
            if (endianess == FileDicomBase.BIG_ENDIAN) {
                intArr[i] = ( (b[offset] & 0xff) << 24) | ( (b[offset + 1] & 0xff) << 16)
                        | ( (b[offset + 2] & 0xff) << 8) | (b[offset + 3] & 0xff);
            } else {
                intArr[i] = ( (b[offset + 3] & 0xff) << 24) | ( (b[offset + 2] & 0xff) << 16)
                        | ( (b[offset + 1] & 0xff) << 8) | (b[offset] & 0xff);
            }
        }

        return intArr;
    }

    /**
     * Converts a little-endian byte array into an array of Longs with length equal to the tag vm.
     * 
     * @param b The byte array.
     * @param vm The tag VM.
     * @param endianess True indicates big endian, false indicates little endian.
     * @return A Long array with length vm.
     */
    public static final Long[] toUInt(final byte[] b, final int vm, final boolean endianess) {
        int offset = 0;
        final Long[] intArr = new Long[vm];
        for (int i = 0; i < vm; i++, offset += 4) {
            if (endianess == FileDicomBase.BIG_ENDIAN) {
                intArr[i] = ( (b[offset] & 0xffL) << 24) | ( (b[offset + 1] & 0xffL) << 16)
                        | ( (b[offset + 2] & 0xffL) << 8) | (b[offset + 3] & 0xffL); // Big Endian
            } else {
                intArr[i] = ( (b[offset + 3] & 0xffL) << 24) | ( (b[offset + 2] & 0xffL) << 16)
                        | ( (b[offset + 1] & 0xffL) << 8) | (b[offset] & 0xffL);
            }
        }

        return intArr;
    }

    /**
     * Converts a little-endian byte array into an array of Integers with length equal to the tag vm.
     * 
     * @param b The byte array.
     * @param vm The tag VM.
     * @param endianess True indicates big endian, false indicates little endian.
     * @return An Integer array (storing short values) with length vm.
     */
    public static final Integer[] toShort(final byte[] b, final int vm, final boolean endianess) {
        int offset = 0;
        final Integer[] shortArr = new Integer[vm];
        for (int i = 0; i < vm; i++, offset += 2) {
            if (endianess == FileDicomBase.BIG_ENDIAN) {
                shortArr[i] = ( (b[offset] & 0xff) << 8) | (b[offset + 1] & 0xff);
            } else {
                shortArr[i] = ( (b[offset + 1] & 0xff) << 8) | (b[offset] & 0xff);
            }

            // signed
            if ( (shortArr[i] & 0x0080) != 0) {
                shortArr[i] = shortArr[i] | 0xff00;
            }
        }

        return shortArr;
    }

    /**
     * Converts a little-endian byte array into an array of Integers with length equal to the tag vm.
     * 
     * @param b The byte array.
     * @param vm The tag VM.
     * @param endianess True indicates big endian, false indicates little endian.
     * @return An Integer array (storing short values) with length vm.
     */
    public static final Integer[] toUShort(final byte[] b, final int vm, final boolean endianess) {
        int offset = 0;
        final Integer[] shortArr = new Integer[vm];
        for (int i = 0; i < vm; i++, offset += 2) {
            if (endianess == FileDicomBase.BIG_ENDIAN) {
                shortArr[i] = ( (b[offset] & 0xff) << 8) | (b[offset + 1] & 0xff);
            } else {
                shortArr[i] = ( (b[offset + 1] & 0xff) << 8) | (b[offset] & 0xff);
            }
        }

        return shortArr;
    }

    /**
     * Converts a little-endian byte array into an array of Floats with length equal to the tag vm.
     * 
     * @param b The byte array.
     * @param vm The tag VM.
     * @param endianess True indicates big endian, false indicates little endian.
     * @return An Float array with length vm.
     */
    public static final Float[] toFloat(final byte[] b, final int vm, final boolean endianess) {
        int offset = 0;
        final Float[] floatArr = new Float[vm];
        for (int i = 0; i < vm; i++, offset += 4) {
            if (endianess == FileDicomBase.BIG_ENDIAN) {
                floatArr[i] = Float.intBitsToFloat( ( ( (b[offset] & 0xff) << 24) | ( (b[offset + 1] & 0xff) << 16)
                        | ( (b[offset + 2] & 0xff) << 8) | (b[offset + 3] & 0xff)));
            } else {
                floatArr[i] = Float.intBitsToFloat( ( ( (b[offset + 3] & 0xff) << 24) | ( (b[offset + 2] & 0xff) << 16)
                        | ( (b[offset + 1] & 0xff) << 8) | (b[offset] & 0xff)));
            }
        }

        return floatArr;
    }

    /**
     * Converts a little-endian byte array into an array of Doubles with length equal to the tag vm.
     * 
     * @param b The byte array.
     * @param vm The tag VM.
     * @param endianess True indicates big endian, false indicates little endian.
     * @return An Double array with length vm.
     */
    public static final Double[] toDouble(final byte[] b, final int vm, final boolean endianess) {
        long b1, b2, b3, b4, b5, b6, b7, b8;
        int offset = 0;
        final Double[] doubleArr = new Double[vm];
        for (int i = 0; i < vm; i++, offset += 8) {
            b1 = (b[offset] & 0xff);
            b2 = (b[offset + 1] & 0xff);
            b3 = (b[offset + 2] & 0xff);
            b4 = (b[offset + 3] & 0xff);
            b5 = (b[offset + 4] & 0xff);
            b6 = (b[offset + 5] & 0xff);
            b7 = (b[offset + 6] & 0xff);
            b8 = (b[offset + 7] & 0xff);

            if (endianess == FileDicomBase.BIG_ENDIAN) {
                doubleArr[i] = Double.longBitsToDouble( ( (b1 << 56) | (b2 << 48) | (b3 << 40) | (b4 << 32)
                        | (b5 << 24) | (b6 << 16) | (b7 << 8) | b8));
            } else {
                doubleArr[i] = Double.longBitsToDouble( ( (b8 << 56) | (b7 << 48) | (b6 << 40) | (b5 << 32)
                        | (b4 << 24) | (b3 << 16) | (b2 << 8) | b1));
            }
        }

        return doubleArr;
    }
    
    @Override
	public int compareTo(FileDicomTag toCompare) {
        int keyCompare = this.getKey().compareTo(toCompare.getKey());
        if(keyCompare == 0) {
        	return getValue(true).toString().compareTo(toCompare.getValue(true).toString());
        } else {
        	return keyCompare;
        }
    }

}
