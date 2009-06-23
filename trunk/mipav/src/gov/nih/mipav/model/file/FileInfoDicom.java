package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.util.*;


/**
 * <p>This class contains DICOM header information. It uses a table to store all the information about the tags. The
 * FileDicomTagTable listing all known tags with empty values is in DicomDictionary. Also stored here is the offset of
 * the image, the image number, the resolutions, some pixel data, the bytes allocated, and the dimensions of the
 * image.</p>
 *
 * <p>This file also contains a table used for displaying the tag information. There is an option to display just the
 * standard tags or both the standard and the private tags.</p>
 *
 * @version  1.0 Aug 1, 1999
 * @see      FileDicom
 * @see      FileDicomTag
 * @see      FileDicomTagTable
 * @see      JDialogFileInfoDICOM
 */
public class FileInfoDicom extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3072660161266896186L;

    /** Used to indicate that the DICOM tags are explicit (i.e they have VRs). */
    public static final boolean EXPLICIT = false;

    /** Used to indicate that the DICOM tags are implicit (i.e. they use the DICOM dictionary). */
    public static final boolean IMPLICIT = true;
    
    /** (0028,0103) Pixel Representations */
    public static final int UNDEFINED_PIXEL_REP = -1;
    
    /** (0028,0103) Pixel Representations */
    public static final int UNSIGNED_PIXEL_REP = 0;
    
    /** (0028,0103) Pixel Representations */
    public static final int SIGNED_PIXEL_REP = 1;

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

    /** Whether the tag currently being processed and read in is a sequence tag. */
    public boolean isCurrentTagSQ = false;

    /** True indicates image has multiple image planes in a single file. */
    public boolean multiFrame = false;

    // Most of this variables are used to simplify access to important tags. Should
    // NOT be used when writing images to file since some of the info may not
    // be up to data. Might clean this up at some time in the future.

    /** True is image format < DICOM 3.0. */
    public boolean olderVersion = false; // an older version is a version < DICOM v3.0

    /** Orientation - SAGITTAL, CORONAL, AXIAL. */
    public String orientation;

    /** Type of image - color, monochrome etc. */
    public String photometricInterp = "MONOCHROME2";

    /**
     * Pixel Padding Value (0028, 0120). Value of pixels added to non-rectangular image to pad to rectangular format.
     */
    public Short pixelPaddingValue = null;

    /** DICOM tag (0028,103) 
     * 	-1 = undefined
     * 	 0 = unsigned 
     *   1 = signed */
    public short pixelRepresentation = UNDEFINED_PIXEL_REP;
    

    /** 0 = RGB, RGB, 1 = R R R, G G G, B B B. */
    public short planarConfig = 0; //

    /** DICOM slice location. */
    public float sliceLocation;

    /** VR type can be IMPLICIT or EXPLICIT. */
    public boolean vr_type = IMPLICIT;

    /** DICOM x coordianate of the slice location. */
    public float xLocation = 0;

    /** DICOM y coordianate of the slice location. */
    public float yLocation = 0;

    /** DICOM z coordianate of the slice location. */
    public float zLocation = 0;

    /** Stores all the information about the DICOM tags. */
    private FileDicomTagTable tagTable;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * DICOM file information constructor. Used for the file info which contains the reference tag table for an image.
     *
     * @param  name       file name
     * @param  directory  file directory
     * @param  format     format (in this case, DICOM)
     */
    public FileInfoDicom(String name, String directory, int format) {
        super(name, directory, format);

        tagTable = new FileDicomTagTable(this);
    }

    /**
     * DICOM file information constructor. Used for the file info which does NOT contain the reference tag table for an
     * image.
     *
     * @param  name       file name
     * @param  directory  file directory
     * @param  format     format (in this case, DICOM)
     * @param  refInfo    The reference file info, containing the reference tag table.
     */
    public FileInfoDicom(String name, String directory, int format, FileInfoDicom refInfo) {
        super(name, directory, format);

        tagTable = new FileDicomTagTable(this, refInfo.getTagTable());
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
                    getTagTable().setValue(anonymizeTagIDs[i], "", 0);
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
            if ((list[i]) && (tagTable.getValue(anonymizeTagIDs[i]) != null)) {
                if(anonymizeTagIDs[i].equals("0008,0018")) { 
                	//though not technically required, nany programs want this field to be populated
                	Random r = new Random();
            		String anonStr = "";
            		String strValue = getTagTable().get("0008,0018").getValue(true).toString();
            		for(int j=0; j<strValue.length(); j++) {
                		if(strValue.charAt(j) == '.') {
                			anonStr = anonStr+".";
                		} else {
                			anonStr = anonStr+r.nextInt(10);
                		} 
                	}
            		getTagTable().setValue(anonymizeTagIDs[i], anonStr);
                } else {
                	getTagTable().setValue(anonymizeTagIDs[i], "", 0);
                }
            }
        }
        // this fileInfo is now an expurgated/sanitised version
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
        orientation = null;
        photometricInterp = null;

        if (tagTable != null) {
            tagTable.reset();
            //tagTable.finalize();
        }

        tagTable = null;

        super.finalize();
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
        int index1, index2, index3, index4, index5, index6;
        int notSet = -1;
        double[][] dirCos = null;
        TransMatrix xfrm = null;

        try {
            dirCos = new double[4][4]; // row, col
            xfrm = new TransMatrix(4);

            String orientation = (String) tagTable.getValue("0020,0037");

            if (orientation == null) {
                Preferences.debug("Error reading tag 0020, 0037 - null \n", Preferences.DEBUG_FILEIO);
                return null;
            }

            index1 = index2 = index3 = index4 = index5 = index6 = notSet;
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
                    } else if (index5 == notSet) {
                        index5 = i;
                    } else {
                        index6 = i;
                        break;
                    }
                }
            }

            dirCos[0][0] = Double.valueOf(orientation.substring(0, index1)).doubleValue();
            dirCos[0][1] = Double.valueOf(orientation.substring(index1 + 1, index2)).doubleValue();
            dirCos[0][2] = Double.valueOf(orientation.substring(index2 + 1, index3)).doubleValue();
            dirCos[0][3] = 0;

            dirCos[1][0] = Double.valueOf(orientation.substring(index3 + 1, index4)).doubleValue();
            dirCos[1][1] = Double.valueOf(orientation.substring(index4 + 1, index5)).doubleValue();
            if (index6 == notSet)
            	dirCos[1][2] = Double.valueOf(orientation.substring(index5 + 1)).doubleValue();
            else
            	dirCos[1][2] = Double.valueOf(orientation.substring(index5 + 1, index6)).doubleValue();
            	
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

            xfrm.copyMatrix(dirCos);
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
     * Returns a reference to the tag table for this dicom file info.
     *
     * @return  A reference to the tag table.
     */
    public final FileDicomTagTable getTagTable() {
        return tagTable;
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
     * Parse the string for Objects seperated by "\". Uses the local method getValue to look at the Object held in the
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
    public final String[] parseTagValue(String tagName) {

        if (tagName == null) {
            return null;
        }

        String str = (String) tagTable.getValue(tagName);

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
     * After the tag table is filled, check for a number of tags to set up some fields in this file info object.
     */
    public final void setInfoFromTags() {

        // type 1
        if (tagTable.containsTag("0008,0060")) {
            setInfoFromTag(tagTable.get("0008,0060"));
        }

        if (tagTable.containsTag("0018,0088")) {
            setInfoFromTag(tagTable.get("0018,0088"));
        }

        if (tagTable.containsTag("0028,0100")) {
            setInfoFromTag(tagTable.get("0028,0100"));
        } else {
            MipavUtil.displayError("FileInfoDicom: tag (0028,0100) is missing a type 1 tag.");
        }

        if (tagTable.containsTag("0028,0103")) {
            setInfoFromTag(tagTable.get("0028,0103"));
        } else {
            MipavUtil.displayError("FileInfoDicom: tag (0028,0103) is missing a type 1 tag.");
        }

        if (tagTable.containsTag("0028,1052")) {
            setInfoFromTag(tagTable.get("0028,1052"));
        }

        if (tagTable.containsTag("0028,1053")) {
            setInfoFromTag(tagTable.get("0028,1053"));
        }

        // type 2, etc
        if (tagTable.containsTag("0018,0050")) {
            setInfoFromTag(tagTable.get("0018,0050"));
        }

        if (tagTable.containsTag("0018,602C")) {
            setInfoFromTag(tagTable.get("0018,602C"));
        }

        if (tagTable.containsTag("0018,602E")) {
            setInfoFromTag(tagTable.get("0018,602E"));
        }

        if (tagTable.containsTag("0020,0032")) {
            setInfoFromTag(tagTable.get("0020,0032"));
        }

        if (tagTable.containsTag("0020,0013")) {
            setInfoFromTag(tagTable.get("0020,0013"));
        }

        if (tagTable.containsTag("0020,1041")) {
            setInfoFromTag(tagTable.get("0020,1041"));
        }

        if (tagTable.containsTag("0028,0030")) {
            setInfoFromTag(tagTable.get("0028,0030"));
        }

        if (tagTable.containsTag("0018,1164")) {
            setInfoFromTag(tagTable.get("0018,1164"));
        }

        if (tagTable.containsTag("0028,0120")) {
            setInfoFromTag(tagTable.get("0028,0120"));
        }
    }

    /**
     * Changes a few dicom tags associated with a secondary capture image (marking it as having been created with
     * mipav). This is often used after a new image is created as the result of an algorithm and the source image's file
     * infos are copied over to the new image.
     */
    public void setSecondaryCaptureTags() {

        // Secondary Capture SOP UID
        getTagTable().setValue("0002,0002", "1.2.840.10008.5.1.4.1.1.7 ", 26);
        getTagTable().setValue("0008,0016", "1.2.840.10008.5.1.4.1.1.7 ", 26);

        // bogus Implementation UID made up by Matt
        getTagTable().setValue("0002,0012", "1.2.840.34379.17", 16);
        getTagTable().setValue("0002,0013", "MIPAV--NIH", 10);
    }

    /**
     * Returns the MIPAV modality constant for a given DICOM modality string.
     *
     * @param   value  The DICOM modality tag value.
     *
     * @return  The equivalent MIPAV modality constant (or UNKNOWN_MODALITY if there is no equivalent found).
     */
    protected static final int getModalityFromDicomStr(String value) {

        if (value.equals("BI")) {
            return BIOMAGENETIC_IMAGING;
        } else if (value.equals("CD")) {
            return COLOR_FLOW_DOPPLER;
        } else if (value.equals("CR")) {
            return COMPUTED_RADIOGRAPHY;
        } else if (value.equals("CT")) {
            return COMPUTED_TOMOGRAPHY;
        } else if (value.equals("DD")) {
            return DUPLEX_DOPPLER;
        } else if (value.equals("DG")) {
            return DIAPHANOGRAPHY;
        } else if (value.equals("DX")) {
            return DIGITAL_RADIOGRAPHY;
        } else if (value.equals("ES")) {
            return ENDOSCOPY;
        } else if (value.equals("GM")) {
            return GENERAL_MICROSCOPY;
        } else if (value.equals("HC")) {
            return HARDCODY;
        } else if (value.equals("IO")) {
            return INTRAORAL_RADIOGRAPHY;
        } else if (value.equals("LS")) {
            return LASER_SURFACE_SCAN;
        } else if (value.equals("MA")) {
            return MAGNETIC_RESONANCE_ANGIOGRAPHY;
        } else if (value.equals("MG")) {
            return MAMMOGRAPHY;
        } else if (value.equals("MR")) {
            return MAGNETIC_RESONANCE;
        } else if (value.equals("MS")) {
            return MAGNETIC_RESONANCE_SPECTROSCOPY;
        } else if (value.equals("NM")) {
            return NUCLEAR_MEDICINE;
        } else if (value.equals("OT")) {
            return OTHER;
        } else if (value.equals("PT")) {
            return POSITRON_EMISSION_TOMOGRAPHY;
        } else if (value.equals("PX")) {
            return PANORAMIC_XRAY;
        } else if (value.equals("RF")) {
            return RADIO_FLUOROSCOPY;
        } else if (value.equals("RG")) {
            return RADIOGRAPHIC_IMAGING;
        } else if (value.equals("RTDOSE")) {
            return RADIOTHERAPY_DOSE;
        } else if (value.equals("RTIMAGE")) {
            return RADIOTHERAPY_IMAGE;
        } else if (value.equals("RTPLAN")) {
            return RADIOTHERAPY_PLAN;
        } else if (value.equals("RTRECORD")) {
            return RADIOTHERAPY_RECORD;
        } else if (value.equals("RTSTRUCT")) {
            return RADIOTHERAPY_STRUCTURE_SET;
        } else if (value.equals("SM")) {
            return SLIDE_MICROSCOPY;
        } else if (value.equals("ST")) {
            return SINGLE_PHOTON_EMISSION_COMPUTED_TOMOGRAPHY;
        } else if (value.equals("TG")) {
            return THERMOGRAPHY;
        } else if (value.equals("US")) {
            return ULTRASOUND;
        } else if (value.equals("XA")) {
            return XRAY_ANGIOGRAPHY;
        } else if (value.equals("XC")) {
            return EXTERNAL_CAMERA_PHOTOGRAPHY;
        } else {
            return UNKNOWN_MODALITY;
        }
    }

    /**
     * Sets fields in this file info based on a given tag's key and value.
     *
     * @param  tag  The tag to use to update the file info fields (not all tags cause field changes).
     */
    protected final void setInfoFromTag(FileDicomTag tag) {
        FileDicomKey tagKey = tag.getInfo().getKey();

        // skip empty values.. (e.g., bad instance number string)
        if ((tag.getValue(false) instanceof String) && tag.getValue(false).equals("")) {
            return;
        }

        // ordering by type 1 tags first.  Then in numerical order.
        if (tagKey.equals("0008,0060")) {
            // type 1

            setModalityFromDicomStr((String) tag.getValue(false));

            // setModalityFromDicomStr() covers the possibility of value == ""
        } else if (tagKey.equals("0018,0088")) {
            // type 1

            super.setResolutions(Float.parseFloat((String) tag.getValue(false)), 2);
        } else if (tagKey.equals("0028,0100")) {
            // type 1

            bitsAllocated = ((Short) tag.getValue(false)).shortValue();
        } else if (tagKey.equals("0028,0103")) {
            // type 1

            pixelRepresentation = ((Short) tag.getValue(false)).shortValue();
        } else if (tagKey.equals("0028,1052")) {
            // type 1

            super.setRescaleIntercept(Double.valueOf((String) tag.getValue(false)).doubleValue());

            // only used in CT images, so don't notify that not found
        } else if (tagKey.equals("0028,1053")) {
            // type 1

            super.setRescaleSlope(Double.valueOf((String) tag.getValue(false)).doubleValue());

            // only used in CT and PET images, so don't notify that not found
        }

        // any variables whose value depends on a type 2 or type 3 tag
        if (tagKey.equals("0018,0050")) {
            // type 2

            setSliceThickness(Float.parseFloat(((String) tag.getValue(false)).trim()));
        } else if (tagKey.equals("0018,602C")) {
            setResolutions(((Double) tag.getValue(false)).floatValue(), 0);
        } else if (tagKey.equals("0018,602E")) {
            setResolutions(((Double) tag.getValue(false)).floatValue(), 1);
        } else if (tagKey.equals("0020,0032")) { // type 2c
            orientation = ((String) tag.getValue(false)).trim();

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
            
            if (index1 != -1)
            	xLocation = Float.valueOf(orientation.substring(0, index1)).floatValue();
            if (index1 != -1 && index2 != -1)
            	yLocation = Float.valueOf(orientation.substring(index1 + 1, index2)).floatValue();
            if (index2 != -1)
            	zLocation = Float.valueOf(orientation.substring(index2 + 1)).floatValue();
            
            if (index1 == -1 || index2 == -1)
            	Preferences.debug("Warning reading tag 0020, 0032 - too few items \n", Preferences.DEBUG_FILEIO);
            
        } else if (tagKey.equals("0020,0013")) { // type 2

            try {
                instanceNumber = Integer.parseInt(((String) tag.getValue(false)).trim());
            } catch (NumberFormatException e) {
                MipavUtil.displayError("FileInfoDicom: Number format error: Instance Number.");
            }
        } else if (tagKey.equals("0020,1041")) { // type 3

            String s = ((String) tag.getValue(false)).trim();

            try {
                Float temp;
                temp = new Float(s);
                sliceLocation = temp.floatValue();
            } catch (NumberFormatException e) {
                MipavUtil.displayError("Number format error: Slice Location = " + s);
            }
        } else if (tagKey.equals("0028,0030") &&
                       ((tagTable.get("0018,1164") == null) || (tagTable.get("0018,1164").getValue(false) == null))) { // type 2

            String valueStr = (String) tag.getValue(false);
            String firstHalf, secondHalf;
            int index = 0;

            for (int i = 0; i < valueStr.length(); i++) {

                if (valueStr.charAt(i) == '\\') {
                    index = i;
                }
            }

            firstHalf = valueStr.substring(0, index).trim();
            secondHalf = valueStr.substring(index + 1, valueStr.length()).trim();

            Float f1 = null;
            Float f2 = null;

            if ((firstHalf != null) && (firstHalf.length() > 0)) {

                try {
                    f1 = new Float(firstHalf);
                } catch (NumberFormatException e) {
                    setResolutions(1.0f, 0);
                    // MipavUtil.displayError("Number format error: Pixel spacing = " + s);
                }

                if (f1 != null) {
                    setResolutions(f1.floatValue(), 0);
                }
            } else {
                setResolutions(1.0f, 0);
            }

            if ((secondHalf != null) && (secondHalf.length() > 0)) {

                try {
                    f2 = new Float(secondHalf);
                } catch (NumberFormatException e) {
                    setResolutions(getResolution(0), 1);
                    // MipavUtil.displayError("Number format error: Pixel spacing = " + s);
                }

                if (f2 != null) {
                    setResolutions(f2.floatValue(), 1);
                }
            } else {
                setResolutions(1.0f, 1);
            }
        } else if (tagKey.equals("0018,1164")) { // type 2

            String valueStr = (String) tag.getValue(false);
            String firstHalf, secondHalf;
            int index = 0;

            for (int i = 0; i < valueStr.length(); i++) {

                if (valueStr.charAt(i) == '\\') {
                    index = i;
                }
            }

            firstHalf = valueStr.substring(0, index).trim();
            secondHalf = valueStr.substring(index + 1, valueStr.length()).trim();

            Float f1 = null;
            Float f2 = null;

            if ((firstHalf != null) && (firstHalf.length() > 0)) {

                try {
                    f1 = new Float(firstHalf);
                } catch (NumberFormatException e) {
                    setResolutions(1.0f, 0);
                    // MipavUtil.displayError("Number format error: Pixel spacing = " + s);
                }

                if (f1 != null) {
                    setResolutions(f1.floatValue(), 0);
                }
            } else {
                setResolutions(1.0f, 0);
            }

            if ((secondHalf != null) && (secondHalf.length() > 0)) {

                try {
                    f2 = new Float(secondHalf);
                } catch (NumberFormatException e) {
                    setResolutions(getResolution(0), 1);
                    // MipavUtil.displayError("Number format error: Pixel spacing = " + s);
                }

                if (f2 != null) {
                    setResolutions(f2.floatValue(), 1);
                }
            } else {
                setResolutions(1.0f, 1);
            }
        } else if (tagKey.equals("0028,0120")) { // type 3
            pixelPaddingValue = (Short) tag.getValue(false);
        }
    }

    /**
     * Uses the DICOM tag value to set the Image modality field.
     *
     * @param  value  Object used is the value of DICOM tag (0008,0060)
     */
    protected final void setModalityFromDicomStr(String value) {
        super.setModality(getModalityFromDicomStr(value));
    }
}
