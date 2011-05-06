package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;


/**
 * Information about a DICOM tag, stored in the DicomDictionary table once for each DICOM tag. This information is read
 * in from the dicom dictionary on disk.
 *
 * @see  DicomDictionary
 * @see  FileDicomKey
 * @see  FileDicomTag
 */
public class FileDicomTagInfo extends ModelSerialCloneable {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3009696612758502873L;

    /** The default DICOM version string to assign to tags. */
    public static final String DEFAULT_TAG_VERSION = "3";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The DICOM tag 'group,element' key indicating what tag this information pertains to. */
    protected FileDicomKey key;

    /** The tag keyword (no spaces). */
    protected String keyword;

    /** The real world DICOM tag name. */
    protected String name;

    /** Allowed value multiplicity (vm) for this tag. */
    protected int valueMultiplicity;

    /** DICOM value representation (vr) for this tag. */
    protected String valueRepresentation;

    /**
     * Version of the tag. Usually '2' or '3', although some tags get strings like 'GEM' or 'TSH' when the tag is from a
     * private manufacturer.
     */
    protected String version;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor that takes all the information needed about the tag except for the value and DICOM revision number
     * (which we assume to be 3).
     *
     * @param  dicomKey  The group and element which uniquely define this tag
     * @param  vr        the value representation
     * @param  vm        the value multiplicity
     * @param  keyword   the keyword (no spaces)
     * @param  name      the real world name
     *
     * @see    #DEFAULT_TAG_VERSION
     */
    public FileDicomTagInfo(FileDicomKey dicomKey, String vr, int vm, String keyword, String name) {
        this(dicomKey, DEFAULT_TAG_VERSION, vr, vm, keyword, name);
    }

    /**
     * Constructor that takes all the information needed about the tag except for the value and DICOM revision number.
     *
     * @param  dicomKey  The group and element which uniquely define this tag
     * @param  version   the DICOM revision (version) number (eg. '3' or 'GEM')
     * @param  vr        the value representation
     * @param  vm        the value multiplicity
     * @param  keyword   the keyword (no spaces)
     * @param  name      the real world name
     */
    public FileDicomTagInfo(FileDicomKey dicomKey, String version, String vr, int vm, String keyword, String name) {
        this.key = dicomKey;
        this.version = version;
        this.valueRepresentation = vr;
        this.valueMultiplicity = vm;
        this.keyword = keyword;
        this.name = name;
    }

    /**
     * Constructor that takes all the information needed about the tag except for the value and DICOM revision number.
     *
     * @param  group    the integer group word (in hexadecimal)
     * @param  element  the integer element word (in hexadecimal)
     * @param  vr       the value representation
     * @param  vm       the value multiplicity
     * @param  keyword  the keyword (no spaces)
     * @param  name     the real world name
     *
     * @see    #DEFAULT_TAG_VERSION
     */
    public FileDicomTagInfo(int group, int element, String vr, int vm, String keyword, String name) {
        this(new FileDicomKey(group, element), vr, vm, keyword, name);
    }

    /**
     * Constructor that takes all the information needed about the tag except for the value.
     *
     * @param  group    the integer group word (in hexadecimal)
     * @param  element  the integer element word (in hexadecimal)
     * @param  version  the DICOM revision (version) number (eg. '3' or 'GEM')
     * @param  vr       the value representation
     * @param  vm       the value multiplicity
     * @param  keyword  the keyword (no spaces)
     * @param  name     the real world name
     */
    public FileDicomTagInfo(int group, int element, String version, String vr, int vm, String keyword, String name) {
        this(new FileDicomKey(group, element), version, vr, vm, keyword, name);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Accessor that returns the type of tag, depending on a value representation value. This allows the reader to read
     * in the tags properly.
     *
     * @param   vr  value representation
     *
     * @return  the type (int, double, short, String, or unknown)
     */
    public static final String getType(String vr) {

        try {

            if (vr.equals("SL") || vr.equals("UL")) { // Signed Long  || Unsigned Long
                return FileDicomBase.TYPE_INT;
            } else if (vr.equals("SS") || vr.equals("US")) { // Signed short || Unsigned short
                return FileDicomBase.TYPE_SHORT;
            } else if (vr.equals("SH") || // Short String
                           vr.equals("DS") || // Decimal String
                           vr.equals("IS") || // Integer String
                           vr.equals("LO") || // Long String
                           vr.equals("ST") || // Short Text
                           vr.equals("LT") || // Long Text
                           vr.equals("CS") || // Code String
                           vr.equals("DA") || // Date
                           vr.equals("DT") || // Date and Time
                           vr.equals("TM") || // Time
                           vr.equals("UI") || // UID
                           vr.equals("PN") || // Person's Name
                           vr.equals("AS") || // Age String
                           vr.equals("AE") || // Application Entity Title
                           vr.equals("UN") || // Unknown
                           vr.equals("UT")) { // Unlimited Text
                return FileDicomBase.TYPE_STRING;
            } else if (vr.equals("FL")) { // Floating Point Single (float)
                return FileDicomBase.TYPE_FLOAT;
            } else if (vr.equals("FD")) { // Floating Point Double
                return FileDicomBase.TYPE_DOUBLE;
            } else if (vr.equals("OW")) {
                return FileDicomBase.OTHER_WORD_STRING;
            } else if (vr.equals("OB")) {
                return FileDicomBase.OTHER_BYTE_STRING;
            } else if (vr.equals("SQ")) {
                return FileDicomBase.TYPE_SEQUENCE;
            } else {
                return FileDicomBase.TYPE_UNKNOWN; // unknown
            }
        } catch (NullPointerException npe) {
            return FileDicomBase.TYPE_UNKNOWN;
        }
    }

    /**
     * Tests whether a tag info object contains the same information as this tag info object.
     *
     * @param   obj  A object (hopefully a non-null FileDicomTagInfo, but not required).
     *
     * @return  Whether all of the tag info (key, keyword, name, vm, vr, version) in the other tag is the same as this
     *          one. False if the tag is null or not a FileDicomTagInfo.
     */
    public boolean equals(Object obj) {

        if ((obj != null) && (obj.getClass() == this.getClass())) {
            FileDicomTagInfo info = (FileDicomTagInfo) obj;

            if (this.key.equals(info.key) && this.keyword.equals(info.keyword) && this.name.equals(info.name)) {

                if ((this.valueMultiplicity == info.valueMultiplicity) && this.version.equals(info.version)) {

                    // handle private tags with an unknown vr
                    if (((this.valueRepresentation == null) && (info.valueRepresentation == null)) ||
                            this.valueRepresentation.equals(info.valueRepresentation)) {
                        return true;
                    }
                }
            }
        }

        return false;
    }

    /**
     * Return the dicom tag key ('group,element') that this information object describes.
     *
     * @return  The dicom tag key described by this information object.
     */
    public FileDicomKey getKey() {
        return key;
    }

    /**
     * Returns the keyword for this tag, a word with no spaces.
     *
     * @return  the keyword
     */
    public final String getKeyword() {
        return keyword;
    }

    /**
     * Accessor that returns the real world name of this tag. For private tags, the name is the value.
     *
     * @return  the name of this tag
     */
    public final String getName() {
        return name;
    }

    /**
     * Accessor that returns the type of tag, depending on the value representation. This allows the reader to read in
     * the tags properly. This method will be so much simpler when (if) the tags are seperated out as individual
     * classes.
     *
     * @return  the type (int, double, short, String, or unknown)
     */
    public final String getType() {
        return getType(getValueRepresentation());
    }

    /**
     * Accessor that returns the value multiplicity of the tag. The value multiplicity is how many instances of this
     * value representation (VR) there can be in one tag.
     *
     * @return  the value multiplicity
     */
    public final int getValueMultiplicity() {
        return valueMultiplicity;
    }

    /**
     * Accessor that returns the value representation of the tag. The value representation allows the reader the read
     * and interpret the tag properly. Because private tags are not unique, the VR is null and they may be read and/or
     * displayed improperly.
     *
     * @return  the value representation
     */
    public final String getValueRepresentation() {
        return valueRepresentation;
    }

    /**
     * Accessor that returns the DICOM version of the tag. Although not needed to understand a value, it is important to
     * the dictionary.
     *
     * @return  the String that is the DICOM version.
     */
    public final String getVersion() {
        return version;
    }

    /**
     * Accessor to the DICOM version number for this tag. Returns it as a number so numerical comparisons (greater-than,
     * equality, etc) can be made. Also, throws a NumberFormatException for private tags.
     *
     * @throws  NumberFormatException  when a tag is a Private Tag.
     *
     * @return  the version number.
     */
    public final int getVersionNumber() throws NumberFormatException {
        return Integer.parseInt(getVersion());
    }

    /**
     * Returns the unique identifier's hash code.
     *
     * @return  The hash code.
     */
    public final int hashCode() {

        // TODO: this might not be a good hash code...
        return key.hashCode() + keyword.hashCode() + name.hashCode() + valueMultiplicity +
               valueRepresentation.hashCode() + version.hashCode();
    }

    /**
     * Sets the dicom tag's key.  This is only used to specify the values of wild card tags stored
     * in the DicomDictionary
     *
     * @param  vr  the value representation
     */
    public final void setKey(FileDicomKey key) {
        this.key = key;
    }
    
    /**
     * Sets the value representation; used only for explicit vr.
     *
     * @param  vr  the value representation
     */
    public final void setValueRepresentation(String vr) {
        this.valueRepresentation = vr;
    }

    /**
     * Return information about the tag, in string form.
     *
     * @return  A tag information string.
     */
    public final String toString() {
        String str = new String();

        if (getVersion() != null) {
            str += " VER: " + getVersion();
        }

        if (getValueRepresentation() != null) {
            str += " VR: " + getValueRepresentation();
        }

        str += " VM: " + getValueMultiplicity();

        if (getKeyword() != null) {
            str += " Keyword: " + getKeyword();
        }

        if (getName() != null) {
            str += " Name: " + getName();
        }

        return str;
    }
    
    
    public void finalize() {
    	
    	key =null;
    	keyword = null;
    	name = null;
    	valueRepresentation = null;
    	version = null;
    	
    }
}
