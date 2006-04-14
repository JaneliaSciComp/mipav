package gov.nih.mipav.model.file;

import gov.nih.mipav.model.structures.ModelSerialCloneable;

import java.util.Hashtable;
import java.util.Enumeration;
import java.util.Vector;

/**
*   This class stores the item in a sequence tag.  The item is just a series of
*   DICOM tags so this storage model is very similar to the FileInfoDicom model.
*   For more information on the sequence tag, see the DICOM standard, Part 5, Section 7.5.
*   For more information on the storage model see DicomSQ.
*
*   @author Neva Cherniavsky
*   @see    FileDicomSQ
*   @see    FileInfoDicom
*   @see    FileDicomTag
*/
public class FileDicomItem extends ModelSerialCloneable {

    private Hashtable dataSet;

    /**
    *   Creates an empty Hashtable for storing the DICOM tags and stores the length of the item.
    */
    public FileDicomItem() {
        dataSet = new Hashtable();
    }

    /**
    *   Prepares this class for cleanup
    */
    public void finalize() {
        if (dataSet != null) {
            dataSet.clear();
        }
        dataSet = null;

        try {super.finalize();}
        catch (Throwable er){}
    }

    /**
    *   Puts a new tag into the dataSet Hashtable
    *   @param key    key to map to in the Hashtable
    *   @param value  value that matches the key
    */
    public final void putTag(Object key, Object value) {
        dataSet.put(key, value);
    }

    /**
    *  Gets the value of a tag within a sequence
    *  @param key key to match
    */
    public final Object getTag(Object key) {
        return dataSet.get(key);
    }

    /**
    *   Returns the hashtable with all the items in it
    *   @return    the dataSet
    */
    public final Hashtable getDataSet() {
        return dataSet;
    }

    /**
    *   Sorts the list of tags and returns it as an array in order
    *   @return        the sorted list
    */
    public FileDicomTag[] sortDataSet() {
        Enumeration e;
        FileDicomTag dicomTags[];

        dicomTags = new FileDicomTag[getNumberOfElements()];

        int i=0;

        for (e=dataSet.keys(); e.hasMoreElements(); ) {
            String name = (String)e.nextElement();
            FileDicomTag element = (FileDicomTag)getTag(name);
            dicomTags[i++] = element;
        }

        FileDicomTag temp;
        for (int p=1; p < dicomTags.length; p++) {
            temp = dicomTags[p];
            int gr = temp.getGroup();
            int el = temp.getElement();
            int j = p;
            for ( ; j > 0 &&
                    (gr < dicomTags[j-1].getGroup() ||
                     (gr == dicomTags[j-1].getGroup() &&
                      el < dicomTags[j-1].getElement()));
                  j--) {
                dicomTags[j] = dicomTags[j-1];
            }
            dicomTags[j] = temp;

        }
        return dicomTags;
    }

    /**
    *   Sets the length of the tag
    *   @param key     key to map to in the Hashtable
    *   @param length  length to set
    */
    public final void setLength(Object key, int length) {
        FileDicomTag entry = (FileDicomTag)dataSet.get(key);
        entry.setLength(length);
    }

    /**
    *   Gets the length of the item
    *   @return the length of the item
    */
    public final int getLength() {
        return getDataLength();
    }

    /** Calculates the number of bytes that the data (the object value) takes
     * to be stored.  This method returns the number of data items times the
     * sizeof the data type.  This method will be so much simpler when (if)
     * the tags are seperated out as individual classes.
     * @return size of the value in bytes
     */
    public int getDataLength() {
        FileDicomTag[] tagslist = sortDataSet();
        int datasize = 0;
        for (int i=0; i < tagslist.length; i++) {
            datasize += tagslist[i].getLength();
        }
        return datasize;
    }

    /**
     *   Gets the number of elements in the data set (# of tags in item)
     *   @return the number of elements
     */
    public final int getNumberOfElements() {
        return dataSet.size();
    }

    /**
    *   Sets the value of the DicomTag in the tagsList Hashtable
    *              with the same hexadecimal tag name.  The tag names are
    *              unique and that's why they are the keys to the Hashtable.
    *   @param name  the key for the DicomTag in dataSet
    *   @param value the value to set the DicomTag to
    */
    public final void setValue(Object name, Object value, int length) {
        FileDicomTag entry = (FileDicomTag)dataSet.get(name);
        entry.setValue(value.toString(), length);
        dataSet.remove(name);
        dataSet.put(name, entry);
    }

    /**
    *   Returns a set of Strings that represent the item in readable
    *   form.  The item is represented as the key, the real world name,
    *   and the value.  The value is separated from the rest by ";;;".
    *   This is so when the tags are displayed in ViewFileInfoDicom, a
    *   StringTokenizer can parse the strings and put the value in a
    *   eparate column.
    *   @return a Vector of the strings
    */
    public Vector getItemDisplay() {
        Vector display = new Vector();
        String value = "";

        for (Enumeration e=dataSet.keys(); e.hasMoreElements(); ) {
            String key = (String)e.nextElement();
            FileDicomTag entry = (FileDicomTag)getTag(key);
            if (entry.getValue(true).toString().equals("Sequence")) {
                value =  "(" + key + "): " + entry.getName() + ";;;" + "Sequence";
                display.addElement(value);

                Vector sqDisplay = ((FileDicomSQ)entry.getValue(true)).getSequenceDisplay();
                for (Enumeration f=sqDisplay.elements(); f.hasMoreElements();){
                    display.addElement(f.nextElement());
                }
            }
            else {
                value = "(" + key + "): " + entry.getName() + ";;;" + entry.getValue(true);
                display.addElement(value);
            }
        }
        return display;
    }

    /**
    *   Converts this object to a String; in this case, just the
    *              word "Item".  The true interpretation is in ViewFileInfoDicom
    *   @return    "Item"
    */
    public String toString() {
        return "Item";
    }
}
