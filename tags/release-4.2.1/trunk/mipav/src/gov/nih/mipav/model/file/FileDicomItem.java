package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import java.io.Serializable;
import java.util.*;


/**
 * This class stores a set of FileDicomTags. This series of DICOM tags is very similar to the FileInfoDicom model. 
 * For more information on the sequence tag, see the DICOM standard, Part 5, Section
 * 7.5. For more information on the storage model see DicomSQ.
 *
 * @author  Neva Cherniavsky
 * @see     FileDicomSQ
 * @see     FileInfoDicom
 * @see     FileDicomTag
 */
public class FileDicomItem extends ModelSerialCloneable {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 788152239624831704L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** TreeMap containing key-tag pairs, string is like a FileDicomKey */
    private TreeMap<String, FileDicomTag> dataSet;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates an empty TreeMap for storing the DICOM tags and stores the length of the item.
     */
    public FileDicomItem() {
        dataSet = new TreeMap<String, FileDicomTag>(new KeyComparator());
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup.
     */
    public void finalize() {

        if (dataSet != null) {
            dataSet.clear();
        }

        dataSet = null;

        try {
            super.finalize();
        } catch (Throwable er) { }
    }

    /**
     * Calculates the number of bytes that the data (the object value) takes to be stored. This method returns the
     * number of data items times the sizeof the data type. This method will be so much simpler when (if) the tags are
     * seperated out as individual classes.
     *
     * @return  size of the value in bytes
     */
    public int getDataLength() {
        Iterator<FileDicomTag> tagsItr = dataSet.values().iterator();
        int datasize = 0;
        
        while(tagsItr.hasNext()) {
        	datasize += tagsItr.next().getLength();
        }

        return datasize;
    }

    /**
     * Returns the hashtable with all the items in it.
     *
     * @return  the dataSet
     */
    public final TreeMap<String, FileDicomTag> getDataSet() {
        return dataSet;
    }

    /**
     * Returns a set of Strings that represent the item in readable form. The item is represented as the key, the real
     * world name, and the value. The value is separated from the rest by ";;;". This is so when the tags are displayed
     * in ViewFileInfoDicom, a StringTokenizer can parse the strings and put the value in a eparate column.
     *
     * @return  a Vector of the strings
     */
    public Vector<String> getItemDisplay() {
        Vector<String> display = new Vector<String>();
        String value = "";
        
        Iterator<String> keyItr = dataSet.keySet().iterator();
        while(keyItr.hasNext()) {
        	String key = keyItr.next();
        	FileDicomTag entry = dataSet.get(key);
        	
        	if (entry.getValue(true).toString().equals("Sequence")) {
                value = "(" + key + "): " + entry.getName() + ";;;" + "Sequence"; //sequence inside a sequence support
                display.addElement(value);
                
                Vector<String> sqDisplay = ((FileDicomSQ) entry.getValue(true)).getSequenceDisplay();
                
                for (Enumeration<String> f = sqDisplay.elements(); f.hasMoreElements();) {
                    display.addElement(f.nextElement());
                }
        	} else {
                value = "(" + key + "): " + entry.getName() + ";;;" + entry.getValue(true);
                display.addElement(value);
            }
        }

        return display;
    }

    /**
     * Gets the length of the item.
     *
     * @return  the length of the item
     */
    public final int getLength() {
        return getDataLength();
    }

    /**
     * Gets the number of elements in the data set (# of tags in item).
     *
     * @return  the number of elements
     */
    public final int getNumberOfElements() {
        return dataSet.size();
    }

    /**
     * Gets the value of a tag within a sequence.
     *
     * @param   key  key to match
     *
     * @return  DOCUMENT ME!
     */
    public final FileDicomTag getTag(String key) {
        return dataSet.get(key);
    }

    /**
     * Puts a new tag into the dataSet TreeMap.
     *
     * @param  key    key to map to in the TreeMap
     * @param  value  value that matches the key
     */
    public final void putTag(String key, FileDicomTag value) {
        dataSet.put(key, value);
    }

    /**
     * Sets the length of the tag.
     *
     * @param  key     key to map to in the TreeMap
     * @param  length  length to set
     */
    public final void setLength(String key, int length) {
        FileDicomTag entry = dataSet.get(key);
        entry.setLength(length);
    }

    /**
     * Sets the value of the DicomTag in the tagsList TreeMap with the same hexadecimal tag name. The tag names are
     * unique and that's why they are the keys to the TreeMap.
     *
     * @param  name    the key for the DicomTag in dataSet
     * @param  value   the value to set the DicomTag to
     * @param  length  DOCUMENT ME!
     */
    public final void setValue(String name, FileDicomTag value, int length) {
        FileDicomTag entry = dataSet.get(name);
        entry.setValue(value.toString(), length);
        dataSet.remove(name);
        dataSet.put(name, entry);
    }

    /**
     * Converts this object to a String; in this case, just the word "Item". The true interpretation is in
     * ViewFileInfoDicom
     *
     * @return  "Item"
     */
    public String toString() {
        return "Item";
    }
    
    /**
     * Compares the keys of two FileDicomItems specified by their keys
     * 
     * @author senseneyj
     *
     */
    private class KeyComparator implements Comparator<String>, Serializable {

		/* (non-Javadoc)
		 * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
		 */
		public int compare(String o1, String o2) {
			if(o1 instanceof String && o2 instanceof String) {
				int o1group = Integer.valueOf(o1.substring(0, 4), 16);
				int o2group = Integer.valueOf(o2.substring(0, 4), 16);
				if(o1group == o2group) {
					int o1element = Integer.valueOf(o1.substring(5), 16);
					int o2element = Integer.valueOf(o2.substring(5), 16);
					return o1element - o2element;
				} 
				return o1group - o2group;

				/*if(((FileDicomKey) o2).getGroupNumber() == ((FileDicomKey) o1).getGroupNumber()) {
					return ((FileDicomKey) o1).getElementNumber() - ((FileDicomKey)o2).getElementNumber();
				}
				return ((FileDicomKey) o1).getGroupNumber() - ((FileDicomKey)o2).getGroupNumber();*/
			} 
			return o1.hashCode() - o2.hashCode();
		}		
	}
}
