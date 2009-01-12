package gov.nih.mipav.model.file;

import gov.nih.mipav.model.structures.*;

import java.util.*;

/**
 * This is a class for reading in a DICOM sequence tag. For more information about the DICOM sequence tag see the DICOM
 * standard, Part 5, Section 7.5.
 *
 * <P>The sequence tag is encoded as follows:</P>
 *
 * <OL>
 *   <LI>A group and element tag indicating that this is a sequence, followed by a length. The group and element tag are
 *     stored in the regular FileInfoDicom object. The length is stored there and also here. Often a sequence will have
 *     an undefined length, which is defined in FileDicom.</LI>
 *   <LI>A series of item tags. Each item tag is read in, the length of the entire item is stored, and the item is read
 *     in. The item is simply a series of DICOM tags with data in them. That information is stored in the class
 *     DicomItem.</LI>
 *   <LI>A tag indicating the end of the sequence.
 *
 *     <P>Given this encoding, the DicomSQ structure is set up as a Vector. The length variable is the length of the
 *     sequence as given in the header and NOT the length of the Vector. The Vector is a series of DicomItems. Each 
 *     DicomItem is a set of tags.</P>
 *   </LI>
 * </OL>
 *
 * @author  Neva Cherniavsky
 * @see     FileDicomItem
 * @see     FileDicom
 * @see     FileInfoDicom
 */
public class FileDicomSQ extends ModelSerialCloneable {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6705966363824982534L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Sequences are composed of items (DICOM items) and store in a vector object. */
    private Vector<FileDicomItem> sequence;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new DicomSQ object with initial length.
     */
    public FileDicomSQ() {
        sequence = new Vector<FileDicomItem>(10, 5);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Add an item to the sequence vector.
     *
     * @param  item  item to add
     */
    public final void addItem(FileDicomItem item) {
        sequence.addElement(item);
    }


    /**
     * Prepares this class for cleanup.
     */
    public void finalize() {

        if (sequence != null) {
            sequence.removeAllElements();
        }

        sequence = null;

        try {
            super.finalize();
        } catch (Throwable er) { }
    }

    /**
     * Returns the size of the data held in this sequence in number of bytes, including the number of bytes required to
     * delimit each item.
     *
     * @return  the size of the data held in this sequence in number of bytes.
     */
    public int getDataLength() {
        int datasize = 0;

        for (int i = 0; i < sequence.size(); i++) {

            // item start delimiter: FE FF 00 E0 00 00 00 00 (item start)
            datasize += 8;

            // call the item's version of this method for each item:
            for (int j = 0; j < sequence.size(); j++) {
                datasize += sequence.get(j).getLength();
            }

            // item end delimiter: FE FF 0D E0 00 00 00 00 (item end)
            datasize += 8;
        }

        // we do not include sequence beginning and ending delimiters.
        return datasize;
    }

    /**
     * Gets the specified item from the sequence vector.
     *
     * @param   index  index in the vector
     *
     * @return  The specified item from the sequence.
     */
    public final FileDicomItem getItem(int index) {
        return sequence.elementAt(index);
    }

    /**
     * Gets the length as read in by the header (possibly undefined).
     *
     * @return  The length of the sequence as read in by the header
     */
    public final int getLength() {
        return getDataLength();
    }

    /**
     * Gets a series of Strings that are the human readable version of the data.
     *
     * @return  A list that contains the human readable form of the sequence data
     */
    public Vector<String> getSequenceDisplay() {

        Vector<String> display = new Vector<String>();

        for (int i = 0; i < sequence.size(); i++) {

            // call the item's version of this method.
            Vector<String> itemDisplay = getItem(i).getItemDisplay();

            for (Enumeration<String> e = itemDisplay.elements(); e.hasMoreElements();) {
                display.addElement(e.nextElement());
            }
        }

        return display;
    }

    /**
     * Gets the length of the sequence vector.
     *
     * @return  the sequence length
     */
    public final int getSequenceLength() {
        return sequence.size();
    }

    /**
     * Returns the word 'Sequence'.
     *
     * @return  The string 'Sequence' (without the quotes).
     */
    public String toString() {
        return "Sequence";
    }
}
