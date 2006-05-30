package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.util.*;


/**
 * This class contains DICOM header information. It uses a Hashtable to store all the information about the tags. The
 * standard Hashtable listing all known tags with empty values is in CreateDICOMFiles. The tagsList Hashtable in this
 * file is initialized to that Hashtable. Then as the tags are read in by FileDicom, they are stored here. Also stored
 * here is the offset of the image, the image number, the resolutions, some pixel data, the bytes allocated, and the
 * dimensions of the image.
 *
 * <p>This file also contains a table used for displaying the tag information. There is an option to display just the
 * standard tags or both the standard and the private tags.</p>
 *
 * @version  1.0 Aug 1, 1999
 * @see      FileDicom
 * @see      FileDicomTag
 * @see      CreateDICOMFiles
 * @see      JDialogFileInfoDICOM
 */
public class FileInfoDicom extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3072660161266896186L;

    /** Used to indicate that the DICOM tags are explicit (i.e they have VRs). */
    public static final boolean EXPLICIT = false;

    /** Used to indicate that the DICOM tags are implicit (i.e. they use the DICOM dictionary).  */
    public static final boolean IMPLICIT = true;

    /**
     * these are the DICOM tag ID numbers corresponding to the tags which are anonymized. They are kept here for
     * simplicity of updating (when it is decided that other tags are must be anonymized) and to the advantage of
     * calling objects.
     */
    public static final String[] anonymizeTagIDs = {
        "0010,0010", // patient name
        "0010,0020", // patient ID
        "0010,0030", // patient's birth date
        "0010,0032", // patient's birth time
        "0010,0040", // patient's sex
        "0010,0050", // patient Insurance plan code sequence
        "0010,1000", // other patient IDs
        "0010,1001", // other patient names
        "0010,1005", // patient's birth name
        "0010,1010", // patient's age
        "0010,1020", // patient's size
        "0010,1030", // patient's weight
        "0010,1040", // patient's address
        "0010,1060", // patient's mother's birth name
        "0010,1090", // medical record locator
        "0010,2154", // patient's telephone numbers
        "0010,2160", // patient ethnic group
        "0010,2180", // occupation
        "0010,21B0", // additional patient's history
        "0010,21D0", // patient's last menstrual date
        "0010,21F0", // patient religious preference
        "0010,4000", // patient comments.

    "0008,0014", // instance creator UID
        "0008,0018", // SOP instance UID
        "0008,0050", // accession number
        "0008,0080", // institution name
        "0008,0081", // institution address
        "0008,0090", // referring physician's name
        "0008,0092", // referring physician's address
        "0008,0094", // referring physician's telephone numbers
        "0008,1010", // station name
        "0008,1030", // study description
        "0008,103E", // series description
        "0008,1040", // institutional department name
        "0008,1048", // physician(s) of record
        "0008,1050", // performing physician's name
        "0008,1060", // name of physician reading study
        "0008,1070", // operator's name
        "0008,1080", // admitting diagnoses description
        "0008,1155", // Referenced SOP instance UID
        "0008,2111", // derivation description

    "0018,1000", // device serial number
        "0018,1030", // protocol name

    "0020,000D", // study instance UID
        "0020,000E", // series instance UID
        "0020,0010", // study ID
        "0020,0052", // frame of reference UID
        "0020,0200", // synchronization frame of reference UID
        "0020,4000", // image comments

        // "0040,0275",// request attributes sequence
        "0040,A124", // UID
        "0040,A730", // content sequence

    "0088,0140", // storage media file-set UID

    "3006,0024", // referenced frame of reference UID
        "3006,00C2", // related frame of reference UID
    };

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Bits allocated per pixel. */
    public short bitsAllocated = -1;

    /** Number of bytes per pixel. */
    public short bytesPerPixel = 1;

    /** True if the DICOM file has the ID DICM at beginning at file pointer 128. */
    public boolean containsDICM = true;

    /** Data type. */
    public int displayType;

    /** DICOM instance number. */
    public int instanceNumber = 1;

    /** True indicates image has multiple image planes in a single file. */
    public boolean multiFrame = false;

    // Most of this variables are used to simplify access to important tags. Should
    // NOT be used when writing images to file since some of the info may not
    // be up to data. Might clean this up at some time in the future.

    /** Offset in the file where the image begins. */
    public int offset = 0;

    /** True is image format < DICOM 3.0. */
    public boolean olderVersion = false; // an older version is a version < DICOM v3.0

    /** Orientation - SAGITTAL, CORONAL, AXIAL. */
    public String orientation;

    /** Type of image - color, monochrome etc. */
    public String photometricInterp = "MONOCHROME2";

    /** Pixel Padding Value (0028, 0120). Value of pixels added to non-rectangular image to pad to rectangular format. */
    public Short pixelPaddingValue = null;

    /** Data type. */
    public short pixelRepresentation = -1;

    /** 0 = RGB, RGB, 1 = R R R, G G G, B B B. */
    public short planarConfig = 0; //

    /** Stores image resolution. */
    public float[] resolutions = new float[3]; // pixel resolution (units are (unit-of-measure per pixel))

    /** DICOM slice location. */
    public float sliceLocation;

    /** DICOM tag indicating spacing between image planes. */
    public float sliceSpacing = 0;

    /** VR is Value Representation and specifies the data type and format of the values. */
    public String VR = "--";

    /** VR type can be IMPLICIT or EXPLICIT. */
    public boolean vr_type = IMPLICIT;

    /** DICOM x coordianate of the slice location. */
    public float xLocation = 0;

    /** DICOM y coordianate of the slice location. */
    public float yLocation = 0;

    /** DICOM z coordianate of the slice location. */
    public float zLocation = 0;


    /**
     * Hashtable to store all the information about the DICOM tags. The hashtable created in the class CreateDICOMFiles
     * and is a list of all the DICOM tags where initially all the tags are empty of values. Then as the tags are read
     * in by FileDicom and stored here the tagList hashtable.
     */
    private Hashtable tagsList = DICOMDictionaryBuilder.getDicomTagTable();

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * DICOM file information constructor. <b>Does not import the DICOM data dictionary and its associated tags
     * list.</b>
     *
     * @param  name       file name
     * @param  directory  file directory
     * @param  format     format (in this case, DICOM)
     */
    public FileInfoDicom(String name, String directory, int format) {
        super(name, directory, format);
        tagsList = DICOMDictionaryBuilder.getDicomTagTable();
        resolutions[0] = (float) 1.0;
        resolutions[1] = (float) 1.0;
        resolutions[2] = (float) 1.0;
        sliceSpacing = (float) 0.0;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * removes any and all personal information in the info that a doctor, patient, researcher may want to have deleted
     * from the image, such as patient name or patient ID number--from this slice given by this fileInfoDicom.
     *
     * <p>In addition, it allows the change or removal of a whole list of information, such as: ("0010,0010") Patient
     * name; ("0010,0020") patient ID number; ("0010,0050") patient Insurance plan code sequence; ("0010,1000") other
     * patient IDs; ("0010,1001") other patient names; ("0010,1040") patient address; ("0010,1060") patient mother's
     * birth name; ("0010,2154") patient telephone numbers; ("0010,2160") patient ethnic group; ("0010,21D0") patient's
     * last menstrual date; ("0010,21F0") patient religious preference; and ("0010,4000") patient comments.</p>
     *
     * @param   list  the list of tags to remove; it MUST correspond to anonymizeTagIDs list (or spurious errors
     *                result), and it must be of the same length, or this will throw an IllegalArgumentException.
     *
     * @throws  IllegalArgumentException  DOCUMENT ME!
     */
    public final void anonymize(boolean[] list) {

        if (list.length != anonymizeTagIDs.length) {
            throw new IllegalArgumentException("anonymize list not of correct size!");
        }

        int i;

        // DICOM type 2 fields. Existance is required, although value is not.
        for (i = 0; i < 2; i++) {

            try {

                if (list[i]) {
                    this.setValue(anonymizeTagIDs[i], "", 0);
                }
            } catch (NullPointerException npe) { // an IllegalArgumentException is probably not right here....
                throw new IllegalArgumentException("(" + anonymizeTagIDs[i] + ") is a required type 2 tag.");
            }
        }

        // all other fields to anonymize are DICOM type 3 fields and
        // are neither required to have an entry nor required to exist
        // in the image info.
        for (i = 2; i < anonymizeTagIDs.length; i++) {

            // change each of the following tags to (empty)
            // if we are asked to anonymize this info and if the tag exists in the hashtable.
            if ((list[i]) && (this.getValue(anonymizeTagIDs[i]) != null)) {
                this.setValue(anonymizeTagIDs[i], "", 0);
            }
        }
        // this fileInfo is now an expurgated/sanitised version
    }

    /**
     * Clone itself in order to save memory.
     *
     * @return  Object
     */
    public Object cloneItself() {
        FileInfoDicom cloned = (FileInfoDicom) super.cloneItself();
        cloned.offset = offset;
        cloned.VR = VR;
        cloned.sliceLocation = sliceLocation;
        cloned.xLocation = xLocation;
        cloned.yLocation = yLocation;
        cloned.zLocation = zLocation;
        cloned.instanceNumber = instanceNumber;
        cloned.orientation = orientation;
        cloned.olderVersion = olderVersion;
        cloned.containsDICM = containsDICM;
        cloned.sliceSpacing = sliceSpacing;
        cloned.pixelRepresentation = pixelRepresentation;
        cloned.pixelPaddingValue = pixelPaddingValue;
        cloned.bitsAllocated = bitsAllocated;
        cloned.bytesPerPixel = bytesPerPixel;
        cloned.photometricInterp = photometricInterp;
        cloned.planarConfig = planarConfig;
        cloned.multiFrame = multiFrame;
        cloned.vr_type = vr_type;
        cloned.displayType = displayType;

        cloned.resolutions = (float[]) (resolutions.clone());
        cloned.tagsList = (Hashtable) (tagsList.clone());

        return cloned;
    }


    /**
     * Super requires displayAboutInfo to be defined display of this class is held by JDialogFileInfoDICOM. Should never
     * be called.
     *
     * @param  dialog
     * @param  matrix
     *
     * @see    JDialogFileInfoDICOM
     */
    public final void displayAboutInfo(JDialogBase dialog, TransMatrix matrix) {
        Preferences.debug("This method should never be called - FileInfoDicom.displayAboutInfo.");
    }

    /**
     * Prepares this class for cleanup.
     */
    public void finalize() {
        resolutions = null;
        orientation = null;
        photometricInterp = null;

        if (tagsList != null) {
            tagsList.clear();
        }

        tagsList = null;

        VR = null;
        super.finalize();
    }

    /**
     * Accessor that returns the DicomTag with a certain hexadecimal key.
     *
     * @throws  NullPointerException  if the Hashtable is (incorrectly) null here.
     *
     * @param   name  the key for this DicomTag
     *
     * @return  the DicomTag matching that key
     */
    public final Object getEntry(String name) {
        FileDicomTag tag;

        try {
            tag = (FileDicomTag) tagsList.get(new FileDicomKey(name));
        } catch (NullPointerException npe) {
            throw new NullPointerException("Tag list is Null");
        }

        return tag;
    }

    /**
     * Accessor that returns the DicomTag with a certain hexadecimal key.
     *
     * @param   key  the key for this DicomTag
     *
     * @return  the DicomTag matching that key
     */
    public final Object getEntry(FileDicomKey key) {
        return tagsList.get(key);
    }


    /**
     * This method extracts directional cosines from the DICOM image header (tag = "0020, 0037"). Since the third row of
     * transformation matrix is not given in the tag but can be derived by taking the cross product. The result is then
     * put into a MIPAV transformation matrix object and returned.
     *
     * @return  the transformation object extracted from the DICOM header. If this tag is null then the class returns
     *          null.
     */
    public final TransMatrix getPatientOrientation() {
        int index1, index2, index3, index4, index5;
        int notSet = -1;
        double[][] dirCos = null;
        TransMatrix xfrm = null;

        try {
            dirCos = new double[4][4]; // row, col
            xfrm = new TransMatrix(4);

            String orientation = (String) ((FileDicomTag) getEntry("0020,0037")).getValue(true);

            if (orientation == null) {

                // MipavUtil.displayError("Patient Orientation string = null");
                return null;
            }

            index1 = index2 = index3 = index4 = notSet = index5 = notSet;

            for (int i = 0; i < orientation.length(); i++) {

                if (orientation.charAt(i) == '\\') {

                    if (index1 == notSet) {
                        index1 = i;
                    } else if (index2 == notSet) {
                        index2 = i;
                    } else if (index3 == notSet) {
                        index3 = i;
                    } else if (index4 == notSet) {
                        index4 = i;
                    } else {
                        index5 = i;
                    }
                }
            }

            dirCos[0][0] = Double.valueOf(orientation.substring(0, index1)).doubleValue();
            dirCos[0][1] = Double.valueOf(orientation.substring(index1 + 1, index2)).doubleValue();
            dirCos[0][2] = Double.valueOf(orientation.substring(index2 + 1, index3)).doubleValue();
            dirCos[0][3] = 0;

            dirCos[1][0] = Double.valueOf(orientation.substring(index3 + 1, index4)).doubleValue();
            dirCos[1][1] = Double.valueOf(orientation.substring(index4 + 1, index5)).doubleValue();
            dirCos[1][2] = Double.valueOf(orientation.substring(index5 + 1)).doubleValue();
            dirCos[1][3] = 0;

            // cross product
            dirCos[2][0] = (dirCos[0][1] * dirCos[1][2]) - (dirCos[0][2] * dirCos[1][1]);
            dirCos[2][1] = (dirCos[0][2] * dirCos[1][0]) - (dirCos[0][0] * dirCos[1][2]);
            dirCos[2][2] = (dirCos[0][0] * dirCos[1][1]) - (dirCos[0][1] * dirCos[1][0]);
            dirCos[2][3] = 0;

            dirCos[3][0] = 0;
            dirCos[3][1] = 0;
            dirCos[3][2] = 0;
            dirCos[3][3] = 1;

            xfrm.timesEquals(Matrix.constructWithCopy(dirCos));

            // if (Preferences.isDebug()) xfrm.print();
            return xfrm;
        } catch (OutOfMemoryError error) {
            dirCos = null;
            xfrm = null;
            System.gc();
            MipavUtil.displayError("FileInfoDicom: " + error);

            return null;
        }
    }

    /**
     * Gets the tag using keyName as the key into the hashtable.
     *
     * @param   keyName  the key name used to index into the hashtable.
     *
     * @return  FileDicomTag the DICOM tag with associated information.
     */
    public final FileDicomTag getTag(String keyName) {
        FileDicomKey key = new FileDicomKey(keyName);

        return (FileDicomTag) tagsList.get(key);
    }

    /**
     * Accessor that returns the Hashtable.
     *
     * @return  the Hashtable
     */
    public final Hashtable getTagsList() {
        return tagsList;
    }

    /**
     * accessor that returns the value matching the key as a meaningful (ie., non-coded) string . This is a direct
     * accessor to <code>getEntry(name).getValue(true)</code>.
     *
     * @param   name  the key to search for
     *
     * @return  the value that this key matches to as a String for output
     *
     * @see     #getEntry(String)
     * @see     FileDicomTag#getValue(boolean)
     */
    public final Object getValue(String name) {
        FileDicomKey key = new FileDicomKey(name);

        return ((FileDicomTag) tagsList.get(key)).getValue(true);
    }

    /**
     * Accessor for the status of this dicom info.
     *
     * @return  boolean <code>true</code> for images that think they are multi-frame.
     */
    public final boolean isMultiFrame() {
        return multiFrame;
    }

    /**
     * parse the string for Objects seperated by "\". Uses the local method getValue to look at the Object held in the
     * tag and decipher it for the user class into an array of strings.
     *
     * <p>parseTagValue has not been updated for the newer versions of DicomTag</p>
     *
     * @param   tagName  The name given as the key to search the Hashtable for.
     *
     * @return  the array of Strings that were coded into the tag; a single string is given if the string in the tag has
     *          no "\" seporators. Each String in the array is a single value, and there are value multiplicity number
     *          of Strings in the array. NOTE: user class must convert this list to the correct type; (which does
     *          indicate user class knows what returned string is) If null then there were zero tokens in the tag
     */
    public final String[] parseTagValue(Object tagName) {

        if (tagName == null) {
            return null;
        }

        String str = (String) getValue((String) tagName);

        if (str == null) {
            return null;
        }

        StringTokenizer strTokeniser = new StringTokenizer(str, "\\");
        int i = 0;
        String[] tokenList;

        if (strTokeniser.countTokens() == 0) {
            tokenList = null;
        } else {
            tokenList = new String[strTokeniser.countTokens()];

            while (strTokeniser.hasMoreTokens()) {
                tokenList[i] = new String(strTokeniser.nextToken());
                i++;
            }
        }

        return tokenList;
    }

    /**
     * Puts a new tag into the tagsList Hashtable.
     *
     * @param  name      key to map to in the Hashtable
     * @param  dicomTag  the DICOM tag that matches the key
     */
    public final void putTag(String name, Object dicomTag) {
        FileDicomKey key = new FileDicomKey(name);
        tagsList.put(key, dicomTag);
    }

    /**
     * Resets the reference to the dictionary. Sets the DICOM tags table, offered by the <CODE>CreateDICOMFiles</CODE>
     * dictionary. Setting the dictionary has the effect of wiping the tags list of its tags as well.
     */
    public final void resetDictionary() {
        tagsList = DICOMDictionaryBuilder.getDicomTagTable();
    }

    /**
     * Sets the length of the tag.
     *
     * @param  name    key to map to in the Hashtable
     * @param  length  length to set
     */
    public final void setLength(String name, int length) {
        FileDicomKey key = new FileDicomKey(name);
        FileDicomTag entry = (FileDicomTag) tagsList.get(key);

        if (entry != null) {
            entry.setLength(length);
        }
    }


    /**
     * Sets the value of the DicomTag in the tagsList Hashtable with the same hexadecimal tag name. The tag names are
     * unique and that's why they are the keys to the Hashtable. This function also sets modality and other important
     * file information.
     *
     * <p>uses the DicomTag method "setValue(obj)" to automagically:</p>
     *
     * <ul>
     *   <li>determine the length of the tag's value,</li>
     *   <li>convert a "human-readable" string value into a DICOM compliant code,</li>
     *   <li></li>
     *   <li>and when value is a string, but the key specifies some other type of data element.</li>
     * </ul>
     *
     * Private tags are ignored, as are "sequence" and "unknown" tags. Throws an exception when the value is too large
     * for the given tag type.
     *
     * @param  name   the key for the DicomTag in tagsList
     * @param  value  the value to set the DicomTag to
     */
    public final void setValue(String name, Object value) {
        this.setValue(new FileDicomKey(name), value);
    }

    /**
     * Sets the value of the DicomTag in the tagsList Hashtable with the same hexadecimal tag name. The tag names are
     * unique and that's why they are the keys to the Hashtable. This function also sets modality and other important
     * file information.
     *
     * <p>uses the DicomTag method "setValue(obj)" to automagically:</p>
     *
     * <ul>
     *   <li>determine the length of the tag's value,</li>
     *   <li>convert a "human-readable" string value into a DICOM compliant code,</li>
     *   <li></li>
     *   <li>and when value is a string, but the key specifies some other type of data element.</li>
     * </ul>
     *
     * Private tags are ignored, as are "sequence" and "unknown" tags. Throws an exception when the value is too large
     * for the given tag type.
     *
     * @param  key    the key for the DicomTag in tagsList
     * @param  value  the value to set the DicomTag to
     */
    public void setValue(FileDicomKey key, Object value) {

        // has problems when saving!  Don't use until this line is removed (and prob fixed)
        // Is the above statement still valid ? 3/3/2003. It is being used by others and appears to work.
        FileDicomTag entry = (FileDicomTag) getEntry(key);

        if (entry.getType().equals("typeSequence") || entry.getType().equals("typeUnknown") ||
                (entry.getKeyword() == null) || (entry.getVR() == null)) {
            return;
        }

        entry.setValue(value);
        tagsList.put(key, entry);

        // ordering by type 1 tags first.  Then in numerical order.
        if (key.equals("0008,0060")) { // type 1
            setModality(value); // setModality() covers the possibility of value == ""
        } else if (key.equals("0028,0100")) { // type 1

            if (!value.toString().equals("")) {
                bitsAllocated = ((Short) value).shortValue();
            } else {
                MipavUtil.displayError("FileInfoDicom: tag (0028,0100) is missing a type 1 tag.");
            }
        } else if (key.equals("0028,0103")) { // type 1

            if (!value.toString().equals("")) {
                pixelRepresentation = ((Short) value).shortValue();
            } else {
                MipavUtil.displayError("FileInfoDicom: tag (0028,0103) is missing a type 1 tag.");
            }
        } else if (key.equals("0028,1052")) { // type 1

            if (!value.toString().equals("")) {
                super.setRescaleIntercept(Double.valueOf((String) value).doubleValue());
            } // only used in CT images, so don't notify that not found
        } else if (key.equals("0028,1053")) { // type 1

            if (!value.toString().equals("")) {
                super.setRescaleSlope(Double.valueOf((String) value).doubleValue());
            } // only used in CT images, so don't notify that not found
        }

        // any variables whose value depends on a type 2 or type 3 tag
        // are left to using default values when value has 0 length (ie, it is "")
        if (!key.equals("")) {

            if (key.equals("0018,0050")) { // type 2
                resolutions[2] = Float.parseFloat(((String) value).trim());
            } else if (key.equals("0020,0032")) { // type 2c
                orientation = ((String) value).trim();

                int index1 = -1, index2 = -1;

                for (int i = 0; i < orientation.length(); i++) {

                    if (orientation.charAt(i) == '\\') {

                        if (index1 == -1) {
                            index1 = i;
                        } else {
                            index2 = i;
                        }
                    }
                }

                xLocation = Float.valueOf(orientation.substring(0, index1)).floatValue();
                yLocation = Float.valueOf(orientation.substring(index1 + 1, index2)).floatValue();
                zLocation = Float.valueOf(orientation.substring(index2 + 1)).floatValue();
            } else if (key.equals("0020,0013")) { // type 2

                try {
                    instanceNumber = Integer.parseInt(((String) value).trim());
                } catch (NumberFormatException e) {
                    MipavUtil.displayError("FileInfoDicom: Number format error: Instance Number.");
                }
            } else if (key.equals("0020,1041")) { // type 3

                String s = ((String) value).trim();

                try {
                    Float temp;
                    temp = new Float(s);
                    sliceLocation = temp.floatValue();
                } catch (NumberFormatException e) {
                    MipavUtil.displayError("Number format error: Slice Location = " + s);
                }
            } else if (key.equals("0028,0030") && (((FileDicomTag) (getEntry("0018,1164"))).getValue(false) == null)) { // type 2

                String firstHalf, secondHalf;
                int index = 0;

                for (int i = 0; i < ((String) value).length(); i++) {

                    if (((String) value).charAt(i) == '\\') {
                        index = i;
                    }
                }

                firstHalf = ((String) value).substring(0, index).trim();
                secondHalf = ((String) value).substring(index + 1, ((String) value).length()).trim();

                Float f1 = null;
                Float f2 = null;

                if ((firstHalf != null) && (firstHalf.length() > 0)) {

                    try {
                        f1 = new Float(firstHalf);
                    } catch (NumberFormatException e) {
                        resolutions[0] = 1.0f;
                        // MipavUtil.displayError("Number format error: Pixel spacing = " + s);
                    }

                    if (f1 != null) {
                        resolutions[0] = f1.floatValue();
                    }
                } else {
                    resolutions[0] = 1.0f;
                }

                if ((secondHalf != null) && (secondHalf.length() > 0)) {

                    try {
                        f2 = new Float(secondHalf);
                    } catch (NumberFormatException e) {
                        resolutions[1] = resolutions[0];
                        // MipavUtil.displayError("Number format error: Pixel spacing = " + s);
                    }

                    if (f2 != null) {
                        resolutions[1] = f2.floatValue();
                    }
                } else {
                    resolutions[1] = 1.0f;
                }
            } else if (key.equals("0018,1164")) { // type 2

                String firstHalf, secondHalf;
                int index = 0;

                for (int i = 0; i < ((String) value).length(); i++) {

                    if (((String) value).charAt(i) == '\\') {
                        index = i;
                    }
                }

                firstHalf = ((String) value).substring(0, index).trim();
                secondHalf = ((String) value).substring(index + 1, ((String) value).length()).trim();

                Float f1 = null;
                Float f2 = null;

                if ((firstHalf != null) && (firstHalf.length() > 0)) {

                    try {
                        f1 = new Float(firstHalf);
                    } catch (NumberFormatException e) {
                        resolutions[0] = 1.0f;
                        // MipavUtil.displayError("Number format error: Pixel spacing = " + s);
                    }

                    if (f1 != null) {
                        resolutions[0] = f1.floatValue();
                    }
                } else {
                    resolutions[0] = 1.0f;
                }

                if ((secondHalf != null) && (secondHalf.length() > 0)) {

                    try {
                        f2 = new Float(secondHalf);
                    } catch (NumberFormatException e) {
                        resolutions[1] = resolutions[0];
                        // MipavUtil.displayError("Number format error: Pixel spacing = " + s);
                    }

                    if (f2 != null) {
                        resolutions[1] = f2.floatValue();
                    }
                } else {
                    resolutions[1] = 1.0f;
                }
            } else if (key.equals("0028,0120")) { // type 3
                pixelPaddingValue = (Short) value;
            }
        }

    }

    /**
     * Sets the value of the DicomTag in the tagsList Hashtable with the same hexadecimal tag name. The tag names are
     * unique and that's why they are the keys to the Hashtable. This function also sets modality and other important
     * file information.
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
     * file information.
     *
     * @param  key     the key for the DicomTag in tagsList
     * @param  value   the value to set the DicomTag to
     * @param  length  the length of the tag
     */
    public final void setValue(FileDicomKey key, Object value, int length) {

        // Key is the hash key and can have values 60xx where xx yet undefined.
        // However the tag defaults to 6000 until change in FileInfoDicom.setvalue()
        FileDicomTag entry = (FileDicomTag) getEntry(key.getKey());

        if (entry == null) {
            return; // Might wish to display error message.
        }

        if (!key.getGroup().equals("7FE0")) {
            String strGroup = key.getGroup(); // name.substring(0,4);
            entry.setGroup(Integer.valueOf(strGroup, 16).intValue());
        }

        entry.setValue(value, length);
        tagsList.put(key, entry);

        // ordering by type 1 tags first.  Then in numerical order.
        if (key.equals("0008,0060")) { // type 1
            setModality(value); // setModality() covers the possibility of value == ""
        } else if (key.equals("0018,0088")) { // type 1
            super.setSliceSpacing(Float.parseFloat((String) value));
        } else if (key.equals("0028,0100")) { // type 1

            if (!value.toString().equals("")) {
                bitsAllocated = ((Short) value).shortValue();
            } else {
                MipavUtil.displayError("FileInfoDicom: tag (0028,0100) is missing a type 1 tag.");
            }
        } else if (key.equals("0028,0103")) { // type 1

            if (!value.toString().equals("")) {
                pixelRepresentation = ((Short) value).shortValue();
            } else {
                MipavUtil.displayError("FileInfoDicom: tag (0028,0103) is missing a type 1 tag.");
            }
        } else if (key.equals("0028,1052")) { // type 1

            if (!value.toString().equals("")) {
                super.setRescaleIntercept(Double.valueOf((String) value).doubleValue());
            } // only used in CT images, so don't notify that not found
        } else if (key.equals("0028,1053")) { // type 1

            if (!value.toString().equals("")) {
                super.setRescaleSlope(Double.valueOf((String) value).doubleValue());
            } // only used in CT and PET images, so don't notify that not found
        }

        // any variables whose value depends on a type 2 or type 3 tag
        // are left to using default values when value has 0 length (ie, it is "")
        if (length != 0) {

            if (key.equals("0018,0050")) { // type 2
                resolutions[2] = Float.parseFloat(((String) value).trim());
            } else if (key.equals("0018,602C")) {
                resolutions[0] = ((Double) value).floatValue();
            } else if (key.equals("0018,602E")) {
                resolutions[1] = ((Double) value).floatValue();
            } else if (key.equals("0020,0032")) { // type 2c
                orientation = ((String) value).trim();

                int index1 = -1, index2 = -1;

                for (int i = 0; i < orientation.length(); i++) {

                    if (orientation.charAt(i) == '\\') {

                        if (index1 == -1) {
                            index1 = i;
                        } else {
                            index2 = i;
                        }
                    }
                }

                xLocation = Float.valueOf(orientation.substring(0, index1)).floatValue();
                yLocation = Float.valueOf(orientation.substring(index1 + 1, index2)).floatValue();
                zLocation = Float.valueOf(orientation.substring(index2 + 1)).floatValue();
            } else if (key.equals("0020,0013")) { // type 2

                try {
                    instanceNumber = Integer.parseInt(((String) value).trim());
                } catch (NumberFormatException e) {
                    MipavUtil.displayError("FileInfoDicom: Number format error: Instance Number.");
                }
            } else if (key.equals("0020,1041")) { // type 3

                String s = ((String) value).trim();

                try {
                    Float temp;
                    temp = new Float(s);
                    sliceLocation = temp.floatValue();
                } catch (NumberFormatException e) {
                    MipavUtil.displayError("Number format error: Slice Location = " + s);
                }
            } else if (key.equals("0028,0030") && (((FileDicomTag) (getEntry("0018,1164"))).getValue(false) == null)) { // type 2

                String firstHalf, secondHalf;
                int index = 0;

                for (int i = 0; i < ((String) value).length(); i++) {

                    if (((String) value).charAt(i) == '\\') {
                        index = i;
                    }
                }

                firstHalf = ((String) value).substring(0, index).trim();
                secondHalf = ((String) value).substring(index + 1, ((String) value).length()).trim();

                Float f1 = null;
                Float f2 = null;

                if ((firstHalf != null) && (firstHalf.length() > 0)) {

                    try {
                        f1 = new Float(firstHalf);
                    } catch (NumberFormatException e) {
                        resolutions[0] = 1.0f;
                        // MipavUtil.displayError("Number format error: Pixel spacing = " + s);
                    }

                    if (f1 != null) {
                        resolutions[0] = f1.floatValue();
                    }
                } else {
                    resolutions[0] = 1.0f;
                }

                if ((secondHalf != null) && (secondHalf.length() > 0)) {

                    try {
                        f2 = new Float(secondHalf);
                    } catch (NumberFormatException e) {
                        resolutions[1] = resolutions[0];
                        // MipavUtil.displayError("Number format error: Pixel spacing = " + s);
                    }

                    if (f2 != null) {
                        resolutions[1] = f2.floatValue();
                    }
                } else {
                    resolutions[1] = 1.0f;
                }

            } else if (key.equals("0018,1164")) { // type 2

                String firstHalf, secondHalf;
                int index = 0;

                for (int i = 0; i < ((String) value).length(); i++) {

                    if (((String) value).charAt(i) == '\\') {
                        index = i;
                    }
                }

                firstHalf = ((String) value).substring(0, index).trim();
                secondHalf = ((String) value).substring(index + 1, ((String) value).length()).trim();

                Float f1 = null;
                Float f2 = null;

                if ((firstHalf != null) && (firstHalf.length() > 0)) {

                    try {
                        f1 = new Float(firstHalf);
                    } catch (NumberFormatException e) {
                        resolutions[0] = 1.0f;
                        // MipavUtil.displayError("Number format error: Pixel spacing = " + s);
                    }

                    if (f1 != null) {
                        resolutions[0] = f1.floatValue();
                    }
                } else {
                    resolutions[0] = 1.0f;
                }

                if ((secondHalf != null) && (secondHalf.length() > 0)) {

                    try {
                        f2 = new Float(secondHalf);
                    } catch (NumberFormatException e) {
                        resolutions[1] = resolutions[0];
                        // MipavUtil.displayError("Number format error: Pixel spacing = " + s);
                    }

                    if (f2 != null) {
                        resolutions[1] = f2.floatValue();
                    }
                } else {
                    resolutions[1] = 1.0f;
                }
            } else if (key.equals("0028,0120")) { // type 3
                pixelPaddingValue = (Short) value;
            }
        }
    }

    /**
     * Sorts the list of tags and returns it as an array in order.
     *
     * @return  the sorted list
     */
    public final FileDicomTag[] sortTagsList() {
        Enumeration e;
        int count = 0;
        FileDicomTag[] dicomTags;

        for (e = tagsList.keys(); e.hasMoreElements();) {
            FileDicomKey name = (FileDicomKey) e.nextElement();

            if (((FileDicomTag) getEntry(name)).getValue(true) != null) {
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

        for (e = tagsList.keys(); e.hasMoreElements();) {
            FileDicomKey name = (FileDicomKey) e.nextElement();
            FileDicomTag element = (FileDicomTag) getEntry(name);

            if (element.getValue(true) != null) {
                dicomTags[i] = element;
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
     * Uses the DICOM tag value to set the Image modality field.
     *
     * @param  value  - Object used is the value of DICOM tag (0008,0060)
     */
    protected final void setModality(Object value) {

        if (value.equals("BI")) {
            super.setModality(BIOMAGENETIC_IMAGING);
        } else if (value.equals("CD")) {
            super.setModality(COLOR_FLOW_DOPPLER);
        } else if (value.equals("CR")) {
            super.setModality(COMPUTED_RADIOGRAPHY);
        } else if (value.equals("CT")) {
            super.setModality(COMPUTED_TOMOGRAPHY);
        } else if (value.equals("DD")) {
            super.setModality(DUPLEX_DOPPLER);
        } else if (value.equals("DG")) {
            super.setModality(DIAPHANOGRAPHY);
        } else if (value.equals("DX")) {
            super.setModality(DIGITAL_RADIOGRAPHY);
        } else if (value.equals("ES")) {
            super.setModality(ENDOSCOPY);
        } else if (value.equals("GM")) {
            super.setModality(GENERAL_MICROSCOPY);
        } else if (value.equals("HC")) {
            super.setModality(HARDCODY);
        } else if (value.equals("IO")) {
            super.setModality(INTRAORAL_RADIOGRAPHY);
        } else if (value.equals("LS")) {
            super.setModality(LASER_SURFACE_SCAN);
        } else if (value.equals("MA")) {
            super.setModality(MAGNETIC_RESONANCE_ANGIOGRAPHY);
        } else if (value.equals("MG")) {
            super.setModality(MAMMOGRAPHY);
        } else if (value.equals("MR")) {
            super.setModality(MAGNETIC_RESONANCE);
        } else if (value.equals("MS")) {
            super.setModality(MAGNETIC_RESONANCE_SPECTROSCOPY);
        } else if (value.equals("NM")) {
            super.setModality(NUCLEAR_MEDICINE);
        } else if (value.equals("OT")) {
            super.setModality(OTHER);
        } else if (value.equals("PT")) {
            super.setModality(POSITRON_EMISSION_TOMOGRAPHY);
        } else if (value.equals("PX")) {
            super.setModality(PANORAMIC_XRAY);
        } else if (value.equals("RF")) {
            super.setModality(RADIO_FLUOROSCOPY);
        } else if (value.equals("RG")) {
            super.setModality(RADIOGRAPHIC_IMAGING);
        } else if (value.equals("RTDOSE")) {
            super.setModality(RADIOTHERAPY_DOSE);
        } else if (value.equals("RTIMAGE")) {
            super.setModality(RADIOTHERAPY_IMAGE);
        } else if (value.equals("RTPLAN")) {
            super.setModality(RADIOTHERAPY_PLAN);
        } else if (value.equals("RTRECORD")) {
            super.setModality(RADIOTHERAPY_RECORD);
        } else if (value.equals("RTSTRUCT")) {
            super.setModality(RADIOTHERAPY_STRUCTURE_SET);
        } else if (value.equals("SM")) {
            super.setModality(SLIDE_MICROSCOPY);
        } else if (value.equals("ST")) {
            super.setModality(SINGLE_PHOTON_EMISSION_COMPUTED_TOMOGRAPHY);
        } else if (value.equals("TG")) {
            super.setModality(THERMOGRAPHY);
        } else if (value.equals("US")) {
            super.setModality(ULTRASOUND);
        } else if (value.equals("XA")) {
            super.setModality(XRAY_ANGIOGRAPHY);
        } else if (value.equals("XC")) {
            super.setModality(EXTERNAL_CAMERA_PHOTOGRAPHY);
        } else {
            super.setModality(UNKNOWN_MODALITY);
        }
    }
}
