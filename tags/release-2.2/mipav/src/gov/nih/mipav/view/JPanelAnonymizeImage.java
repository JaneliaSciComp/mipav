package gov.nih.mipav.view;

import gov.nih.mipav.model.file.*;


/**
 * Custom panel for anonymizing DICOM images. $Logfile: /mipav/src/gov/nih/mipav/view/JPanelAnonymizeImage.java $
 * $Revision: 20 $ $Date: 3/02/06 12:09p $
 */
public class JPanelAnonymizeImage   extends JPanelChecklist {
    // all the tags that contain personal info:
    private FileInfoDicom dicomInfo;
    
    private FileInfoMinc mincInfo;

	/**
     * constructor to build a panel allowing user to find which tags are available to anonymize.
     * <p>
     * by defualt, sets all checkboxes to enabled. To set the checkboxes to image-specific enabled, use setDicomInfo.
     * @see JPanelAnonymizeImage#setDicomInfo(FileInfoDicom)
     */
    public JPanelAnonymizeImage() {
        super();
        checkboxLabels = makeCheckboxLabels();
        setListLength();
        setBorder("Check the fields to anonymize:");
        buildLayout();
	}

	/** made to ensure the listLength gets set. */
    protected void setListLength() {
        listLength = checkboxLabels.length;
    }

    /**
     * allows the checkboxes to be disabled by testing the file info for the tags present.
     * <p>
     * this <i>affects</i> the enabled status of the checkboxes in the list by setting the checkbox when it exists in
     * the file info.
     * @param fid dicom file info
     */
    public void setDicomInfo(FileInfoDicom fid) {
        dicomInfo = fid;
        findVisibleDicom();
        setCheckBoxesEnabled();
    }
    
    /**
     * allows the checkboxes to be disabled by testing the file info for the tags present.
     * <p>
     * this <i>affects</i> the enabled status of the checkboxes in the list by setting the checkbox when it exists in
     * the file info.
     * @param fim minc file info
     */
    public void setMincInfo(FileInfoMinc fim) {
        mincInfo = fim;
        findVisibleMinc();
        setCheckBoxesEnabled();
    }

    /**
     * Creates the list of labels to use in the checkboxes.
     * @see FileInfoDicom#anonymizeTagIDs
     * @return the list of names held in the DICOM dictionary for the tags' keys held in the list. If a name happens to
     *         be unknown for some reason, the value in the list is merely the tag key ID given by
     *         FileInfoDicom.anonymizeTagIDs.
     */
    protected String[] makeCheckboxLabels() {
        // selector for the user to choose which slices to remove. TRUE means remove.
        String list[] = new String[FileInfoDicom.anonymizeTagIDs.length];
        for (int i=0; i < FileInfoDicom.anonymizeTagIDs.length; i++) {
            try {
                FileDicomKey dicomKey = new FileDicomKey(FileInfoDicom.anonymizeTagIDs[i]);
                String tmpStr = DICOMDictionaryBuilder.getName(dicomKey);
                if (tmpStr != null) {
                    list[i] = new String(tmpStr); // just uses name
                }
                else {
                    list[i] = FileInfoDicom.anonymizeTagIDs[i];
                }
            } catch (IllegalArgumentException badTag) {
                list[i] = FileInfoDicom.anonymizeTagIDs[i];
            }
        }
        return list;
    }

    /**
     * only mark a tag to be visible if it exists in the file info
     * @return the state of existance of each element in the FileInfoDicom in the list of tags.
     * @exception NullPointerException when this classes fileInfoDicom is null.
     */
    private boolean[] findVisibleDicom() {
        if (dicomInfo == null) {
            throw new NullPointerException("JPanelAnonymizeImage: dicomInfo is null.");
        }

        // turn all visible tags off to begin
        for (int i=0; i < FileInfoDicom.anonymizeTagIDs.length; i++) {
            setVisibleList(i, false);// visibleList[i] = false;
            try {
                if (dicomInfo.getValue(FileInfoDicom.anonymizeTagIDs[i]) != null) {
                    // only make a tag visible if it exists in the file info
                    setVisibleList(i, true); // visibleTags[i] = true; // if the tag exists, then let the model of
                                                // the checkbox be visible.
                }
            }
            catch (NullPointerException npe) {;
                System.out.println("Tag not found; tag in question is: " + FileInfoDicom.anonymizeTagIDs[i]);
            }
        }

        return getVisible();
    }
    
    /**
     * only mark a tag to be visible if it exists in the file info
     * @return the state of existance of each element in the FileInfoMinc in the list of tags.
     * @exception NullPointerException when this classes fileInfoMinc is null.
     */
    private boolean[] findVisibleMinc() {
        if (mincInfo == null) {
            throw new NullPointerException("JPanelAnonymizeImage: mincInfo is null.");
        }

        // turn all visible tags off to begin
        for (int i=0; i < FileInfoDicom.anonymizeTagIDs.length; i++) {
            setVisibleList(i, false);
            try {
                if (mincInfo.getDicomValue(FileInfoDicom.anonymizeTagIDs[i]) != null) {
                    // only make a tag visible if it exists in the file info
                    setVisibleList(i, true);
                    // if the tag exists, then let the model of the checkbox be visible.
                }
            }
            catch (NullPointerException npe) {;
                System.out.println("Tag not found; tag in question is: " + FileInfoDicom.anonymizeTagIDs[i]);
            }
        }

        return getVisible();
    }
}
