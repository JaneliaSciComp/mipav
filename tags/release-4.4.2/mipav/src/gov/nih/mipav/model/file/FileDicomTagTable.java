package gov.nih.mipav.model.file;


import gov.nih.mipav.view.*;

import java.util.*;


/**
 * A table containing tags unique to a particular file info in an image. Common tags are not stored here and instead
 * should be stored in the reference tag table (unless this is the reference tag table; then they will be here).
 */
public class FileDicomTagTable implements java.io.Serializable, Cloneable {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -7828203960779215940L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** A list of tag tables which point to this as their reference tag table. */
    private FileDicomTagTable[] childTagTables = null;

    /**
     * Whether this tag table is a reference table (meaning it contains all of the tags for a given fileInfo and does
     * not need to refer anywhere else when retrieving values).
     */
    private boolean isReferenceTagTable = false;

    /**
     * The dicom file info that this tag table belongs to. Used to update file info fields based on tag table changes.
     */
    private FileInfoDicom parentFileInfo;

    /**
     * The reference table to check when a tag is not found in this table. If this table is a reference table, then the
     * value should be <code>this</code> and no checking of the referenceTagTable should take place.
     */
    private FileDicomTagTable referenceTagTable;

    /** Tags unique to this slice, or all of the tags for this slice if this is a reference tag table. */
    private Hashtable<FileDicomKey,FileDicomTag> tagTable;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new FileDicomTagTable object. Should be used for the tags from the first slice of an image volume.
     *
     * @param  parent  The dicom file info that this tag table belongs to.
     */
    public FileDicomTagTable(FileInfoDicom parent) {
        tagTable = new Hashtable<FileDicomKey,FileDicomTag>();

        referenceTagTable = null;
        isReferenceTagTable = true;

        parentFileInfo = parent;
    }

    /**
     * Creates a new FileDicomTagTable object. Should <b>not</b> be used for the tags from first slice of an image
     * volume.
     *
     * @param  parent          The dicom file info that this tag table belongs to.
     * @param  firstSliceTags  A reference to the tag table for the first slice of the image volume
     */
    public FileDicomTagTable(FileInfoDicom parent, FileDicomTagTable firstSliceTags) {
        tagTable = new Hashtable<FileDicomKey,FileDicomTag>();

        referenceTagTable = firstSliceTags;
        isReferenceTagTable = false;

        parentFileInfo = parent;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Sorts the list of tags and returns it as an array in order.
     *
     * @param   tagList  A table of tags to sort according to their keys.
     *
     * @return  The sorted list.
     */
    public static final FileDicomTag[] sortTagsList(Dictionary<FileDicomKey,FileDicomTag> tagList) {
        Enumeration<FileDicomKey> e;
        int count = 0;
        FileDicomTag[] dicomTags;

        for (e = tagList.keys(); e.hasMoreElements();) {

            if (tagList.get(e.nextElement()).getValue(true) != null) {
                count++;
            }
        }

        try {
            dicomTags = new FileDicomTag[count];
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of Memory in FileInfoDicom.sortTagsList");

            return null;
        }

        int i = 0;

        for (e = tagList.keys(); e.hasMoreElements();) {
            FileDicomTag tag = tagList.get(e.nextElement());

            if (tag.getValue(true) != null) {
                dicomTags[i] = tag;
                i++;
            }
        }

        FileDicomTag temp;

        for (int p = 1; p < dicomTags.length; p++) {
            temp = dicomTags[p];

            int gr = temp.getGroup();
            int el = temp.getElement();
            int j = p;

            for (;
                     (j > 0) &&
                     ((gr < dicomTags[j - 1].getGroup()) ||
                          ((gr == dicomTags[j - 1].getGroup()) && (el < dicomTags[j - 1].getElement()))); j--) {

                dicomTags[j] = dicomTags[j - 1];
            }

            dicomTags[j] = temp;

        }

        return dicomTags;
    }

    /**
     * Sets the list of tag tables which point to this as their reference tag table.
     *
     * @param  children  A list of tag tables which point to this as their reference tag table.
     */
    public final void attachChildTagTables(FileDicomTagTable[] children) {
        childTagTables = new FileDicomTagTable[children.length];

        for (int i = 0; i < children.length; i++) {
            childTagTables[i] = (FileDicomTagTable) children[i].clone();
        }
    }

    /**
     * Perform a deep copy of this tag table.
     *
     * @return  A new copy of the tag table.
     */
    public Object clone() {
        FileDicomTagTable newTable;

        if (this.isReferenceTagTable) {
            newTable = new FileDicomTagTable(this.parentFileInfo);

            if (this.childTagTables != null) {
                newTable.childTagTables = (FileDicomTagTable[]) this.childTagTables.clone();
            }
        } else {
            newTable = new FileDicomTagTable(this.parentFileInfo, this.referenceTagTable);
        }

        newTable.tagTable = (Hashtable<FileDicomKey,FileDicomTag>) this.tagTable.clone();

        return newTable;
    }

    /**
     * Returns whether this tag table contains a tag with the given key identifier.
     *
     * @param   keyStr  the string representing the key for this tag -- 'group,element'
     *
     * @return  whether a tag matching the given key is contained in this tag table (or the reference tag table).
     */
    public final boolean containsTag(String keyStr) {
        return containsTag(new FileDicomKey(keyStr));
    }

    /**
     * Returns whether this tag table contains a tag with the given key identifier.
     *
     * @param   key  the key for this tag
     *
     * @return  whether a tag matching the given key is contained in this tag table (does not check the reference tag
     *          table).
     */
    public final boolean containsTag(FileDicomKey key) {
        return tagTable.containsKey(key);
    }

    /**
     * Accessor that returns the tag with a certain hexadecimal key.
     *
     * @param   keyStr  the string representing the key for this tag -- 'group,element'
     *
     * @return  the tag matching that key string
     */
    public final FileDicomTag get(String keyStr) {
        return get(new FileDicomKey(keyStr));
    }

    /**
     * Accessor that returns the tag with a certain hexadecimal key.
     *
     * @param   key  the key for this tag
     *
     * @return  the tag matching that key
     */
    public final FileDicomTag get(FileDicomKey key) {
        FileDicomTag tag = (FileDicomTag) tagTable.get(key);

        if (!isReferenceTagTable && (tag == null)) {
            tag = referenceTagTable.get(key);
        }

        // TODO: exception if not found?

        return tag;
    }

    /**
     * Returns a new copy of all of the tags in this table, including tags from the reference tag table.
     *
     * @return  A new copy of all of the dicom tags.
     */
    public final Hashtable<FileDicomKey,FileDicomTag> getTagList() {
	Hashtable<FileDicomKey,FileDicomTag> tagList;

        if (!isReferenceTagTable) {
            tagList = referenceTagTable.getTagList();

            // merge (tags in this table take precidence)
            Enumeration<FileDicomKey> keys = this.tagTable.keys();

            while (keys.hasMoreElements()) {
                FileDicomKey key = keys.nextElement();
                tagList.put(key, get(key));
            }
        } else {
            tagList = (Hashtable<FileDicomKey,FileDicomTag>) this.tagTable.clone();
        }
        
        //pixel data should never be an element in the tag list.
        FileDicomTag t = tagList.remove(new FileDicomKey(0x7FE0,0x0010));

        return tagList;
    }

    /**
     * Returns the value matching the key as a meaningful (ie., non-coded) string . This is the equivalent of calling
     * <code>get(key).getValue(true)</code>.
     *
     * @param   key  the key to search for
     *
     * @return  the value that this key matches to as a String for output
     *
     * @see     FileDicomTag#getValue(boolean)
     */
    public final Object getValue(FileDicomKey key) {
        FileDicomTag tag = get(key);

        if (tag == null) {
            return null;
        }

        return tag.getValue(true);
    }

    /**
     * Returns the value matching the key as a meaningful (ie., non-coded) string . This is the equivalent of calling
     * <code>get(keyStr).getValue(true)</code>.
     *
     * @param   keyStr  the hexidecimal key to search for -- 'group,element'
     *
     * @return  the value that this key matches to as a String for output
     *
     * @see     FileDicomTag#getValue(boolean)
     */
    public final Object getValue(String keyStr) {
        FileDicomTag tag = get(keyStr);

        if (tag == null) {
            return null;
        }

        return tag.getValue(true);
    }

    /**
     * Sets the tags in this tag table to match the tags contained in the tag table of a dicom file info. All tags
     * already in this table are removed (does not effect tags in its reference tag table).
     *
     * @param  srcDicomInfo  The dicom file info to copy tags from.
     */
    public final void importTags(FileInfoDicom srcDicomInfo) {

        reset();

        // TODO: import all of the tags from the src dicom info
        if (isReferenceTagTable) {

            // the reference table needs _all_ of the tags from the src dicom info, not just the ones unique to that
            // slice (in case the src dicom info is non-reference).  this direct assignment works since getTagList()
            // returns a deep copy of the tag Hashtable
            tagTable = srcDicomInfo.getTagTable().getTagList();
        } else {
            Hashtable<FileDicomKey,FileDicomTag> srcTagList = srcDicomInfo.getTagTable().getTagList();
            Enumeration<FileDicomKey> srcTagKeys = srcTagList.keys();

            while (srcTagKeys.hasMoreElements()) {
                put(srcTagList.get(srcTagKeys.nextElement()));
            }
        }
    }

    /**
     * Adds a private tag to the tag table using some dicom info.
     *
     * @param  info  information about the private dicom tag.
     */
    public final void putPrivateTagValue(FileDicomTagInfo info) {
        FileDicomTag tag = new FileDicomTag(info);
        put(tag);
    }

    /**
     * Remove a tag from this tag table (does not affect reference table, if this is a non-reference table). If this is
     * a reference table, the removal is dumb and doesn't copy the tag to the child tag tables.
     *
     * @param  keyStr  the hexidecimal key to search for -- 'group,element'
     */
    public final void removeTag(String keyStr) {
        removeTag(new FileDicomKey(keyStr));
    }

    /**
     * Remove a tag from this tag table (does not affect reference table, if this is a non-reference table). If this is
     * a reference table, the removal is dumb and doesn't copy the tag to the child tag tables.
     *
     * @param  key  the key to search for
     */
    public final void removeTag(FileDicomKey key) {
        tagTable.remove(key);
    }

    /**
     * Remove all of the tags from this table (does not affect reference table, if this is a non-reference table).
     */
    public final void reset() {
        Enumeration<FileDicomKey> keys = tagTable.keys();

        while (keys.hasMoreElements()) {
            tagTable.remove(keys.nextElement());
        }
        tagTable.clear();
    }

    /**
     * Sets the length of the tag.
     *
     * @param  key     the key to search for
     * @param  length  length to set
     */
    public final void setLength(FileDicomKey key, int length) {
        FileDicomTag entry = get(key);

        if (entry != null) {
            entry.setLength(length);
        }
    }

    /**
     * Sets the length of the tag.
     *
     * @param  keyStr  the hexidecimal key to search for -- 'group,element'
     * @param  length  length to set
     */
    public final void setLength(String keyStr, int length) {
        setLength(new FileDicomKey(keyStr), length);
    }

    /**
     * Sets the value of the DicomTag in the tag table with the same hexadecimal tag name. The tag names are unique and
     * that's why they are the keys to the Hashtable.
     *
     * <p>Uses the FileDicomTag method <code>setValue(obj)</code> to automagically:</p>
     *
     * <ul>
     *   <li>determine the length of the tag's value,</li>
     *   <li>convert a 'human-readable' string value into a DICOM compliant code,</li>
     *   <li>and when value is a string, but the key specifies some other type of data element.</li>
     * </ul>
     *
     * <p>Private tags are ignored, as are 'sequence' and 'unknown' tags. Throws an exception when the value is too
     * large for the given tag type.</p>
     *
     * <p>Should not be used for the values of private tags, unless they are already in the tag table (through a call to
     * putPrivateTagValue()).</p>
     *
     * @param  name   the key string for the FileDicomTag in the tag table -- 'group,element'
     * @param  value  the value to set the DicomTag to
     */
    public final void setValue(String name, Object value) {
        this.setValue(new FileDicomKey(name), value);
    }

    /**
     * Sets the value of the DicomTag in the tag table with the same hexadecimal tag name. The tag names are unique and
     * that's why they are the keys to the Hashtable.
     *
     * <p>Uses the FileDicomTag method <code>setValue(obj)</code> to automagically:</p>
     *
     * <ul>
     *   <li>determine the length of the tag's value,</li>
     *   <li>convert a 'human-readable' string value into a DICOM compliant code,</li>
     *   <li>and when value is a string, but the key specifies some other type of data element.</li>
     * </ul>
     *
     * <p>Private tags are ignored, as are 'sequence' and 'unknown' tags. Throws an exception when the value is too
     * large for the given tag type.</p>
     *
     * <p>Should not be used for the values of private tags, unless they are already in the tag table (through a call to
     * putPrivateTagValue()).</p>
     *
     * @param  key    the key for the FileDicomTag in the tag table
     * @param  value  the value to set the DicomTag to
     */
    public void setValue(FileDicomKey key, Object value) {

        // has problems when saving!  Don't use until this line is removed (and prob fixed)
        // Is the above statement still valid ? 3/3/2003. It is being used by others and appears to work.
        FileDicomTag tag = (FileDicomTag) tagTable.get(key.getKey());

        // no need to add the value to the tag table if it is the same as the reference table; just set the file info
        // data
        if ((tag != null) && isTagSameAsReferenceTag(tag)) {
            parentFileInfo.setInfoFromTag(tag);

            return;
        }

        if (tag == null) {
            FileDicomTagInfo info = DicomDictionary.getInfo(key);

            if (info == null) {

                // TODO: might want to display an error message... the key is not in the dicom dictionary.  how should
                // private, unknown, and sequence tags be handled?
                return;
            } else {
                //is is required if DicomDictionary contains wild card characters
                info.setKey(key);
            }

            tag = new FileDicomTag(info, value);

            put(tag);
        }

        FileDicomTagInfo info = tag.getInfo();

        if (info.getType().equals("typeSequence") || info.getType().equals("typeUnknown") ||
                (info.getKeyword() == null) || (tag.getValueRepresentation() == null)) {
            return;
        }

        tag.setValue(value);

        parentFileInfo.setInfoFromTag(tag);

        // we may have added the tag as an explicit/private tag before setting the value.  remove unneccessary
        // duplication..
        if (isTagSameAsReferenceTag(tag)) {
            this.tagTable.remove(key);
        }
    }

    /**
     * Sets the value of the DicomTag in the tagsList Hashtable with the same hexadecimal tag name. The tag names are
     * unique and that's why they are the keys to the Hashtable. This function also sets modality and other important
     * file information. Should not be used for the values of private tags, unless they are already in the tag table
     * (through a call to putPrivateTagValue()).
     *
     * @param  name    the key for the DicomTag in tagsList (Group, element)
     * @param  value   the value to set the DicomTag to
     * @param  length  the length of the tag
     */
    public final void setValue(String name, Object value, int length) {
        this.setValue(new FileDicomKey(name), value, length);
    }

    /**
     * Sets the value of the DicomTag in the tagsList Hashtable with the same hexadecimal tag name. The tag names are
     * unique and that's why they are the keys to the Hashtable. This function also sets modality and other important
     * file information. Should not be used for the values of private tags, unless they are already in the tag table
     * (through a call to putPrivateTagValue()).
     *
     * @param  key     the key for the DicomTag in tagsList
     * @param  value   the value to set the DicomTag to
     * @param  length  the length of the tag
     */
    public final void setValue(FileDicomKey key, Object value, int length) {

        // Key is the hash key and can have values 60xx where xx yet undefined.
        // However the tag defaults to 6000 until change in FileInfoDicom.setvalue()
        FileDicomTag tag = (FileDicomTag) tagTable.get(key.getKey());

        // no need to add the value to the tag table if it is the same as the reference table; just set the file info
        // data
        if ((tag != null) && isTagSameAsReferenceTag(tag)) {
            parentFileInfo.setInfoFromTag(tag);

            return;
        }

        if (tag == null) {
            FileDicomTagInfo info = DicomDictionary.getInfo(key);
            

            if (info == null) {

                // TODO: might want to display an error message... the key is not in the dicom dictionary.  how should
                // private, unknown, and sequence tags be handled?
                return;
            } else {
                //is is required if DicomDictionary contains wild card characters
                info.setKey(key);
            }

            tag = new FileDicomTag(info);
            tag.setValue(value, length);

            put(tag);
        }

        if (!key.getGroup().equals("7FE0")) {
            String strGroup = key.getGroup();

            // name.substring(0,4);
            tag.setGroup(Integer.valueOf(strGroup, 16).intValue());
        }

        tag.setValue(value, length);

        parentFileInfo.setInfoFromTag(tag);

        // we may have added the tag as an explicit/private tag before setting the value.  remove unneccessary
        // duplication..
        if (isTagSameAsReferenceTag(tag)) {
            this.tagTable.remove(key);
        }
    }

    /**
     * Returns a string containing information about this tag table.
     *
     * @return  The contents of this tag table.
     */
    public String toString() {
        String str = new String();

        str += "is reference table:\t" + isReferenceTagTable + "\n";

        FileDicomTag[] sortedTags = FileDicomTagTable.sortTagsList(tagTable);

        for (int i = 0; i < sortedTags.length; i++) {
            str += sortedTags[i] + "\n";
        }

        return str;
    }

    /**
     * Returns true if a given tag is in the reference tag table and has the same value as the tag in the reference
     * table. Returns false if called from the reference tag table.
     *
     * @param   tag  The tag to check against the reference tag table.
     *
     * @return  Whether the tag is the same as the one in the reference tag table, false otherwise or if this is the
     *          reference tag table.
     */
    protected final boolean isTagSameAsReferenceTag(FileDicomTag tag) {
        return !isReferenceTagTable && referenceTagTable.containsTag(tag.getInfo().getKey()) &&
                   referenceTagTable.get(tag.getInfo().getKey()).equals(tag);
    }

    /**
     * Adds a tag to the tag table, overwriting if a value is already in the table under this tag's key. If this is a
     * reference tag table, then the current value in the tag table must be copied over to all of the tag tables which
     * refer to this one.
     *
     * @param  tag  A tag to add to the table.
     */
    protected final void put(FileDicomTag tag) {
        FileDicomKey key = tag.getInfo().getKey();

        if (isReferenceTagTable) {

            // try to copy the reference value over to all children tables, if they don't have a value for the key
            // already
            if (childTagTables != null) {

                if (containsTag(key) && !get(key).equals(tag)) {
                    FileDicomTag oldTag = get(key);

                    for (int i = 0; i < childTagTables.length; i++) {

                        if (!childTagTables[i].containsTag(key)) {
                            childTagTables[i].put(oldTag);
                        }
                    }
                }
            }

            tagTable.put(key, tag);
        } else {

            // only add the tag if it is not in the reference table and the value we are adding is not the same as the
            // one in the reference table
            if (isTagSameAsReferenceTag(tag)) {
                return;
            }

            tagTable.put(key, tag);
        }
    }
    
    
    /**
     * Prepares this class for cleanup.
     */
    public void finalize() {
    	if(childTagTables != null) {
	    	for(int i=0;i<childTagTables.length;i++) {
	    		if(childTagTables[i] != null) {
	    			childTagTables[i].reset();
	    			childTagTables[i].finalize();
	    		}
	    		childTagTables[i] = null;
	    	}
    	}
    	parentFileInfo = null;
    	if(tagTable != null) {
    		Enumeration<FileDicomKey> keys = tagTable.keys();

            while (keys.hasMoreElements()) {
                tagTable.remove(keys.nextElement());
            }
    		tagTable.clear();
    	}
        tagTable = null;



    }
}
