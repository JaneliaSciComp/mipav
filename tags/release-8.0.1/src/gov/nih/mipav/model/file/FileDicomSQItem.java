package gov.nih.mipav.model.file;

import gov.nih.mipav.model.file.FileInfoDicom.VRtype;

/**
 * A table containing dicom tags. Common tags are not stored here and instead
 * should be stored in the reference tag table.  The reference tag table may refer
 * to another table within a dicom sequence.
 */
public class FileDicomSQItem extends FileDicomTagTable {

    /** Whether the sequence should be written using an unknown length */
    private boolean writeAsUnknownLength = false;
    
    public FileDicomSQItem(FileInfoDicom parent, VRtype vr_type) {
        super(parent, vr_type);
    }

    public FileDicomSQItem(FileInfoDicom parent,
            FileDicomTagTable firstSliceTags, VRtype vr_type) {
        super(parent, firstSliceTags, vr_type);
    }

    /**
     * Whether the sequence should be written using an unknown length, can be set as a preference by user.
     */
    public boolean doWriteAsUnknownLength() {
        return writeAsUnknownLength;
    }

    /**
     * Whether the sequence should be written using an unknown length, this includes adding
     * a sequence delimitation item to the sequence.
     */
    public void setWriteAsUnknownLength(boolean writeAsUnknownLength) {
        this.writeAsUnknownLength = writeAsUnknownLength;
    }
    
    /**
     * Gets the length as read in by the header (possibly undefined).
     *
     * @return  The length of the sequence as read in by the header
     */
    public final int getWritableLength(boolean includeTagInfo) {
        if(doWriteAsUnknownLength()) {
            return -1;
        }
        return getDataLength(includeTagInfo);
    }
}
