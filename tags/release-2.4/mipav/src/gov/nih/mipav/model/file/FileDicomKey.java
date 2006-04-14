package gov.nih.mipav.model.file;

import java.lang.String;
import gov.nih.mipav.model.structures.ModelSerialCloneable;

/**
*   FileDicomKey goes along with FileDicomTag - it is the key
*   into the hashtable that corresponds to a tag.  For most of
*   the tags it is simply a String that is the unique combination
*   of group name and element name (e.g., "0002,0010").  But there
*   are some tags given in the element dictionary as "50xx,0010" or "60xx,0050".
*   The "x"s are wildcards meaning that there can be any number of
*   group names that start with "50" or "60".  In this case we want
*   the hashtable to recognize the tag in order to get the pertinent
*   information from elmdict (such as "Overlay Origin" and what type
*   of data this is) while still storing the tags as unique.  This is
*   accomplished through this class and some code in
*   FileInfoDicom.getEntry(String).
*   @author Neva Cherniavsky
*   @see    FileDicomTag
*   @see    FileInfoDicom
*/
public class FileDicomKey extends ModelSerialCloneable {

    private String key;

    /**
     * Creates new key with the given String as
     * the unique identifier.  The string is checked for proper format,
     * that it specifies 2 hexadecimal strings, separated by a comma.
     * @param s    Unique identifier.
     * @throws IllegalArgumentException if s is null, or formatted incorrectly.
     */
    public FileDicomKey(String s) {
        if (verify(s)) {
            key = s;
        }
        else {
            throw new IllegalArgumentException(s + " cannot represent a DICOM key");
        }
    }

    /** Creates a Key from the given group and element numbers provided.
     */
    public FileDicomKey(int group, int element)
        throws NumberFormatException
    {
        String gr = Integer.toHexString(group).toUpperCase();
        String el = Integer.toHexString(element).toUpperCase();

        while (gr.length() < 4) {
            gr = "0" + gr;
        }

        while (el.length() < 4) {
            el = "0" + el;
        }

        key = gr + "," + el;
    }

    /**
    *   Prepares this class for cleanup
    */
    public void finalize() {
        key = null;

        try {super.finalize();}
        catch (Throwable er){}
    }

    /** Checks that the String s indeed refers to a String that is a valid
     * key, two comma-separated hexidecimal numbers in the format of
     * &quot;[four digit Group number],[four digit element number]&quot;.  The
     * Group number must be four digits, but the final 2 digits may be the
     * character &quot;xx&quot;, which is used to represent a variable
     * group.
     *
     * @param s a String which could be a
     * @return true if the string input can be used to represent a DICOM key,
     *          false if it cannot.
     * @throws IllegalArgumentException when the inputted string cannot be
     *         used to represent a DICOM key.
     */
    public static boolean verify(String s) {
        // the checking we do here verifies that 's' has a valid format.
        // we won't actually change the string 's'; just throw it out if
        // it is incorrect, and use it as 'key' if it is correct.

        if (s == null)
            throw new IllegalArgumentException("String null");

        int commaplace = s.indexOf(',');
        if (commaplace == -1) {
            throw new IllegalArgumentException("Not a DICOM tag Key");
        }

        int group, element;
        try {group = Integer.parseInt(s.substring(0, commaplace), 0x10);}
        catch (NumberFormatException badNumber) {
            String groupString = s.substring(0, commaplace);
           if (groupString.indexOf("xx") == -1) {
               throw new IllegalArgumentException("Not a DICOM tag group");
           }
        }

        try {element = Integer.parseInt(s.substring(commaplace+1), 0x10);}
        catch (NumberFormatException badElement) {
            throw new IllegalArgumentException("Not a DICOM element");
        }

        if (s.substring(0, commaplace).length() != 4 &&
            s.substring(commaplace+1).length() != 4)
        {
            throw new IllegalArgumentException("DICOM tag Keys not valid");
        }

        return true;
    }

    /**
    *   Gets the unique identifier.
    *   @return The unique identifier.
    */
    public final String getKey() {
        return key;
    }

    /**
    *   Sets the unique identifier.
    *   @param key  The unique identifier.
    */
    public final void setKey(String key) {
        if (verify(key)) {
            this.key = key;
        }
        else {
            throw new IllegalArgumentException(key + " cannot represent a DICOM key");
        }
    }

    /**
     *  Objects are equal if both are of type FileDicomKey, and both refer
     *  to the same value; if the input type is a String and the String
     *  representation of the key matches.  This equals method will check
     *  that the keys are exactly are equal or they are equal except for
     *  place-holding &quot;x&quot; characters.
     *
     *  @param obj  Object to compare to.
     *  @return     <code>true</code> if equal.
     */
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj != null && obj instanceof FileDicomKey) {
            char x, y;
            FileDicomKey anotherKey = (FileDicomKey)obj;
            if (anotherKey.getKey().equals(key)) {
                return true;
            }
            else if (key.indexOf('x') != -1 || anotherKey.getKey().indexOf('x') != -1) {
                for (int i=0; i<key.length(); i++) {
                    x = key.charAt(i);
                    y = anotherKey.getKey().charAt(i);
                    if (x != 'x' && y != 'x') {
                        if (x != y) return false;
                    }
                }
                return true;
            }
        }

        if (obj != null && obj instanceof String) {
            char x, y;
            String anotherKey = (String)obj;

            if (anotherKey.equalsIgnoreCase(key)) {
                return true;
            }
            else if (key.indexOf('x') != -1 || anotherKey.indexOf('x') != -1) {
                for (int i=0; i<key.length(); i++) {
                    x = key.charAt(i);
                    y = anotherKey.charAt(i);
                    if (x != 'x' && y != 'x') {
                        if (x != y) {
                            return false;
                        }
                    }
                }
                return true;
            }
        }
        return false;
    }

    /**
    *   Returns the unique identifier.
    *   @return The unique identifier.
    */
    public String toString() {
        return key;
    }

    /** returns only the group portion of the DICOM key.
     * @return The group portion of the DICOM key.
     */
    public final String getGroup() {
        return key.substring(0, key.indexOf(','));
    }

    /** returns only the element portion of the DICOM key.
     * @return The Element portion of the DICOM key.
     */
    public final String getElement() {
        return key.substring( key.indexOf(',') + 1 );
    }

    /** returns the group portion of the DICOM key as an integer.
     * @return The Group number as an integer of the key.
     *  @throws NumberFormatException if the element is a partially
     *          defined value and cannot be converted without some idea of what
     *          the number really is, and therefor some effort.  Group numbers
     *           may be only partially specified (eg., "60xx").
     */
    public final int getGroupNumber() throws NumberFormatException {
        return Integer.parseInt(getGroup(), 0x10);
    }

    /** returns the element portion of a DICOM key as an integer.
     * @return the element number as an integer of the key.
     * @throws NumberFormatException if the element is a partially
     *     defined value and cannot be converted without some effort
     *     (eg, the element number is ("51xx")).  This is possible, but should
     *     not be discovered in elements, only in groups.
     * @see #getGroupNumber()
     */
    public final int getElementNumber()  {
        return Integer.parseInt(getElement(), 0x10);
    }

    /**
    *   Returns the unique identifier's hash code.
    *   @return The hash code.
    */
    public final int hashCode() {
        return key.hashCode();
    }
}
