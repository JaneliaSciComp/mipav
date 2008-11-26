import java.io.IOException;
import java.io.File;
import java.io.RandomAccessFile;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.DicomDictionary;
import gov.nih.mipav.model.file.FileBase;
import gov.nih.mipav.model.file.FileDicomKey;
import gov.nih.mipav.model.file.FileDicomTagInfo;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileUtility;
//import gov.nih.mipav.model.file.FileDicom;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

/**
 * @author joshim2
 * Plugin to anonymize a DICOM image by reading into the header, anonymize certain PII tags,
 * and write back the image. 
 */
public class PlugInGenericAnonymizeDICOM extends AlgorithmBase{
	
	// ~ Instance fields ---------------------------------------------------------------------------------------
	/** File selected by the user */
	private File[] selectedFiles;
	
	/** Additional tag list provided in the dialog */
	private String[] tagListFromDialog;
	
	/** File name of selected file */
	private String selectedFileName;
	
	/** Directory of the selected file */
	private String selectedFileDir;
	
	/** Destination file */
	private File destFile;
	
	/** File type of the selected file */
	private int fileType;
		
	/** ID Offset 128 byte preamble followed by 'D','I','C','M' */
    private final int ID_OFFSET = 132;
    
    /** Boolean identifying if "DICM" is present in the header */
    private boolean containsDICM = false;
    
    /** Endianess (Big or Little), false = little endian, true = big endian, default is set to Little Endian */
    private boolean bigEndian = false;
    
    /** Implicit or Explicit (true if implicit, false if explicit),
     *  initially set to false because (0002) group is always explicit */
    private boolean isImplicit = false; 
    
    /** Buffer pointer (file pointer) */
    private int bPtr;
    
    /** File pointer to the source file. */
    private RandomAccessFile raFile = null;
    
    /** File pointer to the destination file. */
	private RandomAccessFile destRaFile = null;
	
	/** Total length of image file */
	private long fLength;
	
	/** The buffer that holds the tags of the DICOM image. */
    protected byte[] tagBuffer = null;
    
    /** Flag to indicate that the fLength is more than what the tagBuffer can hold. */
    private boolean bufferOverflow = false;
    
    /** Number of bytes following the File Meta Element (end of value field) up to and including the last File Meta
     * Element of the Group 2 File Meta Information. */
    private int metaGroupLength = 0;
    
    /** First number (DICOM group) in ordered pair of numbers that uniquely identifies a data element. */
    private int groupWord;
    
    /** Second number (DICOM element in a group) in ordered pair of numbers that uniquely identifies a data element. */
    private int elementWord;
    
    /** Length of the value field of data element. */
    private int elementLength;
    
    /** Integer variable used to read/write in data so that they don't need to be allocated with each read/write. */
    private int b1, b2, b3, b4, b5, b6, b7, b8;
    
    /** Four byte array used to read/write in data so that one doesn't need to be allocated with each read/write. */
    private byte[] byteBuffer4 = new byte[4];
    
    /** Value Representation - see FileInfoDicom. */
    private byte[] vr = new byte[2];
    
    /** The tag marking the start of the image data. */
    private static final String IMAGE_TAG = "7FE0,0010";
    
    /** The tag marking the beginning of a dicom sequence. */
    private static final String SEQ_ITEM_BEGIN = "FFFE,E000";

    /** The tag marking the end of a dicom sequence. */
    private static final String SEQ_ITEM_END = "FFFE,E00D";

    /** The tag marking the end of an undefined length dicom sequence. */
    private static final String SEQ_ITEM_UNDEF_END = "FFFE,E0DD";
    
    /** Undefined element length. */
    private final int UNDEFINED_LENGTH = -2;
    
    
	
	//	~ Constructors -----------------------------------------------------------------------------------------
	public PlugInGenericAnonymizeDICOM(File[] inputFiles, String[] tagList) {
		
		selectedFiles = inputFiles;
		
		//selectedFileName = selectedFile.getName();
		//selectedFileDir = selectedFile.getParent() + File.separator;
		tagListFromDialog = tagList;
		
	}
	
	//  ~ Methods ----------------------------------------------------------------------------------------------
	
	/**
     * Prepares this class for destruction.
     */
    public void finalize() {
        
    	selectedFiles = null;
               
    }
    
    /** 
     * Starts the algorithm
     */ 
    public void runAlgorithm() {
    	
    	if (selectedFiles == null) {
    		displayError("Selected file is null.");
    		return;
    	} else {
    		
    		int numOfFiles = selectedFiles.length;
    		
    		for (int i = 0; i < numOfFiles; i++) {
    			selectedFileName = selectedFiles[i].getName();
    			selectedFileDir = selectedFiles[i].getParent() + File.separator;
    			
    			try {
        			if (FileUtility.isDicom(selectedFileName, selectedFileDir, false) == FileUtility.DICOM) {
        				containsDICM = true;
        				loadTagBuffer(selectedFiles[i]);
        				anonymizeDicom();
        			} else if (FileUtility.isDicom_ver2(selectedFileName, selectedFileDir, false) == FileUtility.DICOM) {
        				containsDICM = false;
        				loadTagBuffer(selectedFiles[i]);
        				anonymizeDicom(); 
        			} else {
        				displayError("Selected file is not a valid DICOM image.");
        				return;
        			}
        		} catch (IOException ioe) {}
    		}
    	}
    }
     
    /**
     * Setups the allocation of memory for the byte buffer to load the entire image.
     */
    public void loadTagBuffer(File file) {
    	
    	fireProgressStateChanged(0, null, "Loading tag buffer...");
        try {
            raFile = new RandomAccessFile(file, "r");
            if (raFile != null) {
                fLength = raFile.length();
            } else {
                return;
            }

            if (tagBuffer == null) {
                tagBuffer = new byte[(int) fLength];
            } else if (fLength > tagBuffer.length) {
                tagBuffer = new byte[(int) fLength];
                bufferOverflow = true;
            }

            raFile.readFully(tagBuffer);
            raFile.close();
        } catch (IOException ioE) { }
        fireProgressStateChanged(10, null, "Reading DICOM header and anonymizing tags...");
        
    }
    
    
    
    public boolean anonymizeDicom() {
    	boolean flag = true; // Set to false when the tag marking the start of image data is read. 
    	String type = null; // Type of data, see FileInfoDicom.
    	String name = null; // String representing the tag. 
    	String strValue = null;
    	Object data = null;
    	int tagVM = 0;
    	int progressCount = 0;
    	   	
    	String destFName = "Anonymize" + selectedFileName;
    	String destFDir = selectedFileDir;
    	
    	metaGroupLength = 0;
    	elementLength = 0;
    	
    	
    	if (containsDICM) {
    		bPtr = ID_OFFSET;
    	} else {
    		bPtr = 0;
    	}
    	
    	while (flag == true) {
    		
    		progressCount += 1;
    		if ((progressCount % 2) == 0) {
    			fireProgressStateChanged((10 + progressCount), null, "Reading DICOM header and anonymizing tags...");
    		}
    		
    		try {
    			getNextElement();
    			name = convertGroupElement(groupWord, elementWord);
    			    			
    			FileDicomKey key = new FileDicomKey(name);
    			    			
    			if (isImplicit || (groupWord == 2)) {
    				
    				// implicit VR means VR is based on tag as defined in dictionary
                    type = DicomDictionary.getType(key);
                    tagVM = DicomDictionary.getVM(key);
                   // the tag was not found in the dictionary..
                    if (type == null) {
                        type = "typeUnknown";
                        tagVM = 0;
                    }
    			} else { //explicit vr.
    				type = FileDicomTagInfo.getType(new String(vr));
    				
    				if ( !DicomDictionary.containsTag(key)) {
    					tagVM = 0;
    				} else {
    					FileDicomTagInfo info = (FileDicomTagInfo) DicomDictionary.getInfo(key);
    					tagVM = info.getValueMultiplicity();
    				}
    			}
    		    			
    			if (type.equals("typeString")) {
    				strValue = getString(elementLength);
    			} else if (type.equals("otherByteString")) {
    				
    				if (!name.equals(IMAGE_TAG)) {
    					data = getByte(tagVM, elementLength);
    				}
    			} else if (type.equals("otherWordString") && !name.equals("0028,1201") && !name.equals("0028,1202")
                        && !name.equals("0028,1203")) {

                    if ( !name.equals(IMAGE_TAG)) {
                        data = getByte(tagVM, elementLength);
                    }
                } else if (type.equals("typeShort")) {
                    data = getShort(tagVM, elementLength);
                } else if (type.equals("typeInt")) {
                    data = getInteger(tagVM, elementLength);
                } else if (type.equals("typeFloat")) {
                    data = getFloat(tagVM, elementLength);
                } else if (type.equals("typeDouble")) {
                    data = getDouble(tagVM, elementLength);
                }
    			
    		} catch (IOException ioe) {}
    		
    		if (name.equals("0002,0000")) { // File meta elements group length 
    			if (data != null) {
                    metaGroupLength = ((Integer) (data)).intValue() + 12; // 12 is the length of 0002,0000 tag
                }
    		} else if (name.equals("0002,0010")) { // Read Transfer Syntax UID
    			
    			// Transfer Syntax UID: DICOM part 10 page 13, part 5 p. 42-48, Part 6 p. 53
                // 1.2.840.10008.1.2 Implicit VR Little Endian (Default)
                // 1.2.840.10008.1.2.1 Explicit VR Little Endian
                // 1.2.840.10008.1.2.2 Explicit VR Big Endian
                // 1.2.840.10008.1.2.4.50 8-bit Lossy JPEG (JPEG Coding Process 1)
                // 1.2.840.10008.1.2.4.51 12-bit Lossy JPEG (JPEG Coding Process 4)
                // 1.2.840.10008.1.2.4.57 Lossless JPEG Non-hierarchical (JPEG Coding Process 14)
                // we should bounce out if we don't support this transfer syntax
    			if (strValue.trim().equals("1.2.840.10008.1.2")) {
                    bigEndian = false; // Set endianness
    				isImplicit = true; // Set implicit or explicit
                } else if (strValue.trim().equals("1.2.840.10008.1.2.1")) {
                    bigEndian = false;
                    isImplicit = false;
                } else if (strValue.trim().equals("1.2.840.10008.1.2.2")) {
                    bigEndian = true;
                    isImplicit = false;
                } else if (strValue.trim().startsWith("1.2.840.10008.1.2.4.")) { // JPEG
                    bigEndian = false;
                    isImplicit = false;
                } else {
                	MipavUtil.displayError("MIPAV does not support transfer syntax:\n" + strValue);
                	flag = false;
                	return false;
                }
    			
    			setFilePointer(ID_OFFSET + metaGroupLength);
    		}
    		
    		if (name.equals(IMAGE_TAG)) {
    			flag = false;
    		}
    		
    		if (tagExistInAnonymizeTagIDs(name)) {
    			
    			anonymizeTag(name, elementLength, type, data, strValue, tagVM);
    		}
    		
    	}
    	
    	try {
    		
    		destFile = new File(destFDir + destFName);
    		raFile = new RandomAccessFile(destFile, "rw");
    		raFile.seek(0);
    		raFile.write(tagBuffer);
    		raFile.close();
    		tagBuffer = null;
    		fireProgressStateChanged(100, null, "Reading DICOM header and anonymizing tags...");
    	} catch (NullPointerException npe) {
    		MipavUtil.displayError("Pathname argument is null while creating a new file.");
    	} catch (IOException ioe) {
    		MipavUtil.displayError("Error writing file.");
    	}
    	return true;
    }
    
    /** Search the tag name for its existence in the lookup table of tags to be anonymized and the user provided
     * tag list from dialog. 
     * See FileInfoDicom for the detailed list of tags. 
     * @param tagName Name of the tag to search e.g "0002,0000"
     * @return true if name exists, false if the name does not exist. 
     */ 
    
    private boolean tagExistInAnonymizeTagIDs(String tagName) {
    	int len = FileInfoDicom.anonymizeTagIDs.length;
    	
    	if (tagListFromDialog != null) {
    		int lenTagList = tagListFromDialog.length;
    		
    		for (int j = 0; j < lenTagList; j++) {
        		
        		if (tagListFromDialog[j].equals(tagName)) {
        			return true;
        		}
        	}
    	}
    	
    	if (len == 0) {
    		return false;
    	}
    	
    	for (int i = 0; i < len; i++) {
    		
    		if (FileInfoDicom.anonymizeTagIDs[i].equals(tagName)) {
    			return true;
    		}
    		
    	}
    	
    	return false;
    }
    
    private void anonymizeTag(String tagName, int length, String tagType, Object tagData, String strValue, int vm ) {
    	
    	System.out.print(tagName + ";");
    	System.out.print(tagType + ";");
    	if (strValue != null) {
    		System.out.println(strValue);
    	} else {
    		for (int i = 0; i < vm; i++) {
    			System.out.println(tagData);
    		}
    	}
    	
    	System.out.println(strValue);
    	
    	
    	if (tagType == "typeString") {
    		if (strValue != null) {
    			anonymizeString("x", length);
    		}
    	} else if (tagType == "otherByteString" || tagType == "otherWordString") {
    		anonymizeOtherByteString("x", length);
    	} else if (tagType == "typeShort") {
    		anonymizeTypeShort((short) 0, length);
    	} else if (tagType == "typeInt") {
    		anonymizeTypeInt(0, length);
    	} else if (tagType == "typeFloat") {
    		anonymizeTypeFloat(0.0f, length);
    	} else if (tagType == "typeDouble") {
    		anonymizeTypeDouble(0.0, length);
    	}
    	
    	
    	
    }
    
    private void anonymizeString(String strValue, int len) {
    	
    	String str = strValue;
    	
    	for (int i = 0; i < (len-1); i++) {
    		str = str + "x";
    	}
    	
    	byte[] b = str.getBytes();
    	
    	setFilePointer(getFilePointer() - len); 
    	
    	System.arraycopy(b, 0, tagBuffer, bPtr, b.length);
    	bPtr += b.length;
     	
    }
    
    private void anonymizeOtherByteString(String strValue, int len) {
    	
    	String str = strValue;
    	byte[] b = str.getBytes();
    	
    	setFilePointer(getFilePointer() - len);
    	
    	for (int i = 0; i < len; i++) {
    		System.arraycopy(b, 0, tagBuffer, bPtr, b.length);
        	bPtr += b.length;
    	}
       	
    }
    
    private void anonymizeTypeShort (short shValue, int len) {
    	
    	short sh = shValue;
    	byte[] byteBuffer2 = new byte[2];
    	
    	setFilePointer(getFilePointer() - len);
    	
    	for (int i = 0; i < (len/2); i++) {
    		if (bigEndian) {
        		byteBuffer2[0] = (byte) (sh >>> 8);
        		byteBuffer2[1] = (byte) (sh & 0xff);
        		System.arraycopy(byteBuffer2, 0, tagBuffer, bPtr, byteBuffer2.length);
        		bPtr += byteBuffer2.length;
        	} else {
        		byteBuffer2[0] = (byte) (sh & 0xff);
                byteBuffer2[1] = (byte) (sh >>> 8);
                System.arraycopy(byteBuffer2, 0, tagBuffer, bPtr, byteBuffer2.length);
        		bPtr += byteBuffer2.length;
        	}
    	}
    }
    
    private void anonymizeTypeInt (int value, int len) {
    	
    	int intValue = value;
    	byte[] byteBuffer4 = new byte[4];
    	
    	setFilePointer(getFilePointer() - len);
    	
    	for (int i = 0; i < (len/4); i++) {
    		if (bigEndian) {
        		byteBuffer4[0] = (byte) (intValue >>> 24);
        		byteBuffer4[1] = (byte) (intValue >>> 16);
        		byteBuffer4[2] = (byte) (intValue >>> 8);
        		byteBuffer4[3] = (byte) (intValue & 0xff);
        		System.arraycopy(byteBuffer4, 0, tagBuffer, bPtr, byteBuffer4.length);
        		bPtr += byteBuffer4.length;
        	} else {
        		byteBuffer4[0] = (byte) (intValue & 0xff24);
        		byteBuffer4[1] = (byte) (intValue >>> 8);
        		byteBuffer4[2] = (byte) (intValue >>> 16);
        		byteBuffer4[3] = (byte) (intValue >>> 24);
        		System.arraycopy(byteBuffer4, 0, tagBuffer, bPtr, byteBuffer4.length);
        		bPtr += byteBuffer4.length;
        	}
    	}
    }
    
    private void anonymizeTypeLong (long lValue, int len) {
    	
    	long longValue = lValue;
    	byte[] byteBuffer8 = new byte[8];
    	
    	setFilePointer(getFilePointer() - len);
    	
    	for (int i = 0; i < (len/8); i++) {
    		if (bigEndian) {
                byteBuffer8[0] = (byte) (longValue >>> 56);
                byteBuffer8[1] = (byte) (longValue >>> 48);
                byteBuffer8[2] = (byte) (longValue >>> 40);
                byteBuffer8[3] = (byte) (longValue >>> 32);
                byteBuffer8[4] = (byte) (longValue >>> 24);
                byteBuffer8[5] = (byte) (longValue >>> 16);
                byteBuffer8[6] = (byte) (longValue >>> 8);
                byteBuffer8[7] = (byte) (longValue & 0xff);
                System.arraycopy(byteBuffer8, 0, tagBuffer, bPtr, byteBuffer8.length);
                bPtr += byteBuffer8.length;
            } else {
                byteBuffer8[0] = (byte) (longValue & 0xff);
                byteBuffer8[1] = (byte) (longValue >>> 8);
                byteBuffer8[2] = (byte) (longValue >>> 16);
                byteBuffer8[3] = (byte) (longValue >>> 24);
                byteBuffer8[4] = (byte) (longValue >>> 32);
                byteBuffer8[5] = (byte) (longValue >>> 40);
                byteBuffer8[6] = (byte) (longValue >>> 48);
                byteBuffer8[7] = (byte) (longValue >>> 56);
                System.arraycopy(byteBuffer8, 0, tagBuffer, bPtr, byteBuffer8.length);
                bPtr += byteBuffer8.length;
            }
    	}
    	
    	
    }
    
    private void anonymizeTypeFloat (float value, int len) {
    	int tmpInt;
    	float floatValue = value;
    	tmpInt = Float.floatToIntBits(floatValue);
    	anonymizeTypeInt(tmpInt, len);
    }
    
    private void anonymizeTypeDouble(double dValue, int len) {
    	long tmpLong;
    	double doubleValue = dValue;
    	tmpLong = Double.doubleToLongBits(doubleValue);
    	anonymizeTypeLong(tmpLong, len);
    }
    
    
    
       
    
    /**
     * Increments the location, then reads the elementWord, groupWord, and elementLength. It also tests for an end of
     * file, the tag marking the start of image data and resets the elementWord if it encounters one.
     * 
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @throws IOException
     */
    
    private void getNextElement() throws IOException {
    	
    	groupWord = getUnsignedShort();
    	elementWord = getUnsignedShort();
    	
    	read(byteBuffer4);
    	elementLength = getLength(byteBuffer4[0], byteBuffer4[1], byteBuffer4[2], byteBuffer4[3]);
    }
    
    private int getLength(byte b1, byte b2, byte b3, byte b4) throws IOException {
    	
    	if (isImplicit) { // implicit VR with 32-bit length
    		
    		if (bigEndian) {
    			return ( ( (b1 & 0xff) << 24) + ( (b2 & 0xff) << 16) + ( (b3 & 0xff) << 8) + (b4 & 0xff));
    		} else {
    			return ( (b1 & 0xff) + ( (b2 & 0xff) << 8) + ( (b3 & 0xff) << 16) + ( (b4 & 0xff) << 24));
    		}
    		
    	} else if (( (b1 == 79) && (b2 == 66)) || ( (b1 == 79) && (b2 == 87)) || ( (b1 == 83) && (b2 == 81))
                || ( (b1 == 85) && (b2 == 78)) || ( (b1 == 85) && (b2 == 84))) {
    		// Check for VR = 'OB', or 'OW' or 'SQ' or 'UN' or 'UT', that means 32-bit length explicit VR. 
    		// VR = 'OB', or 'OW' or 'SQ' or 'UN' or 'UT'
            vr[0] = (byte) b1;
            vr[1] = (byte) b2;
    		return getInt();
    		
    	} else { // explicit VR with 16-bit length
    		if (bigEndian) {
    			return ( ( (b3 & 0xff) << 8) | (b4 & 0xff));
    		} else {
    			return ( (b3 & 0xff) | ( (b4 & 0xff) << 8));
    		}
    	}
    }
    
    /**
     * Gets the file/buffer pointer and returns it.
     *
     * @return  the file/buffer pointer
     */
    private final int getFilePointer() {
        return bPtr;
    }
    
    /**
     * Sets the file/buffer pointer and returns it.
     */
    private void setFilePointer(int offset) {
        bPtr = offset;
    }
    
    /**
     * Reads a length of the data and deposits it into a single Short or an array of Short as needed by the tag's VM.
     * 
     * @return Object
     * 
     * @throws IOException DOCUMENT ME!
     * 
     * @param vm value multiplicity of the DICOM tag data. VM does not represent how many to find.
     * @param length number of bytes to read out of data stream; the length is not used.
     */
    private Object getShort(int vm, int length) throws IOException {
        int len = (elementLength == UNDEFINED_LENGTH) ? 0 : elementLength;
        int i = 0;
        Object readObject = null; // the Object we read in

        if (vm > 1) {
            Short[] array = new Short[length / 2];

            while (len > 0) { // we should validate with VM here too
                array[i] = new Short((short) getUnsignedShort());
                len -= 2;
                i++;
            }

            readObject = array;
        } else if ( ( (vm < 1) && (length > 2))) {

            // not a valid VM, but we don't initialise the VM to 1,
            // so we will use this fact to guess at valid data.
            // we actually do it as above.
            Short[] array = new Short[length / 2];

            while (len > 0) { // we should validate with VM here too
                array[i] = new Short((short) getUnsignedShort());
                len -= 2;
                i++;
            }

            readObject = array;
        } else if ( ( (vm == 1) && (length > 2))) {

            // not a valid VM, but we don't initialise the VM to 1,
            // so we will use this fact to guess at valid data.
            // we actually do it as above.
            readObject = new Short((short) getUnsignedShort());
            len -= 2;

            while (len > 0) { // we should validate with VM here too
                getUnsignedShort();
                len -= 2;
                i++;
            }
        } else if (length == 2) {
            readObject = new Short((short) getUnsignedShort());
        }

        return readObject;
    }
    
    
    
    /**
     * Reads two unsigned bytes from file.
     *
     * @return     The value of unsigned short read from the file returned as an int.
     *
     * @exception  IOException  if there is an error reading the file
     */
    private final int getUnsignedShort() throws IOException {
        b3 = 0;

        if (bigEndian) {
            b3 = ((tagBuffer[bPtr] & 0xff) << 8) | (tagBuffer[bPtr + 1] & 0xff); // Big Endian
        } else {
            b3 = ((tagBuffer[bPtr + 1] & 0xff) << 8) | (tagBuffer[bPtr] & 0xff); // Little Endian
        }

        bPtr += 2;

        return b3;
    }
    
    /**
     * Reads into the supplied buffer data from the DICOM tag buffer.
     *
     * @param  byteBuffer  byte[]
     */
    private final void read(byte[] byteBuffer) {

        System.arraycopy(tagBuffer, bPtr, byteBuffer, 0, byteBuffer.length);
        bPtr += byteBuffer.length;
    }
    
    /**
     * Converts the integer values of the group word and element word into a string that is the hexadecimal
     * representation of group word and element word, separated by a comma.
     * 
     * @param groupWord The group word of the element.
     * @param elementWord The element word of the element.
     * 
     * @return String representation of the group element.
     */
    private String convertGroupElement(int groupWord, int elementWord) {
        String first, second;

        first = Integer.toString(groupWord, 16);

        while (first.length() < 4) { // prepend with '0' as needed
            first = "0" + first;
        }

        first = first.toUpperCase();
        second = Integer.toString(elementWord, 16);
                                         
        while (second.length() < 4) { // prepend with '0' as needed
            second = "0" + second;
        }

        second = second.toUpperCase();

        return (first + "," + second); // name is the hex string of the tag
    }
    
    /**
     * Reads a length of the data and deposits it into a single Integer or an array of Integer as needed by the tag's
     * VM.
     * 
     * @return Object
     * 
     * @throws IOException DOCUMENT ME!
     * 
     * @param vm value multiplicity of the DICOM tag data. VM does not represent how many to find.
     * @param length number of bytes to read out of data stream; the length is not used.
     */
    private Object getInteger(int vm, int length) throws IOException {
        int len = (elementLength == UNDEFINED_LENGTH) ? 0 : elementLength;
        int i = 0;
        Object readObject = null;

        if (vm > 1) {
            Integer[] array = new Integer[length / 4];

            while (len > 0) { // we should validate with VM here too
                array[i] = new Integer(getInt());
                len -= 4;
                i++;
            }

            readObject = array;
        } else if ( (vm < 1) && (length > 4)) {

            // not a valid VM, but we don't initialise the VM to 1,
            // so we will use this fact to guess at valid data.
            // we actually do it as above.
            Integer[] array = new Integer[length / 4];

            while (len > 0) {
                array[i] = new Integer(getInt());
                len -= 4;
                i++;
            }

            readObject = array;
        } else if ( ( (vm == 1) && (length > 4))) {

            // not a valid VM, but we don't initialise the VM to 1,
            // so we will use this fact to guess at valid data.
            // we actually do it as above.
            readObject = new Integer(getInt());
            len -= 4;

            while (len > 0) { // we should validate with VM here too
                getInt();
                len -= 4;
                i++;
            }
        } else if (length == 4) {
            readObject = new Integer(getInt());
        }

        return readObject;
    }
    
    /**
     * Reads four signed bytes from file.
     *
     * @return     The value of the integer read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    private final int getInt() throws IOException {

        b3 = 0;

        if (bigEndian) {
            b3 = ((tagBuffer[bPtr] & 0xff) << 24) | ((tagBuffer[bPtr + 1] & 0xff) << 16) |
                     ((tagBuffer[bPtr + 2] & 0xff) << 8) | (tagBuffer[bPtr + 3] & 0xff); // Big Endian
        } else {
            b3 = ((tagBuffer[bPtr + 3] & 0xff) << 24) | ((tagBuffer[bPtr + 2] & 0xff) << 16) |
                     ((tagBuffer[bPtr + 1] & 0xff) << 8) | (tagBuffer[bPtr] & 0xff); // Little Endian
        }

        bPtr += 4;

        return b3;
    }
    
    
    /**
     * Reads a string from a file of given <code>length</code>.
     *
     * @param      length  Number of bytes that form the string.
     *
     * @return     The string read from the tag buffer.
     *
     * @exception  IOException  if there is an error reading the file
     */
    private final String getString(int length) throws IOException {

        if (length <= 0) {
            return new String("");
        }

        byte[] b = new byte[length];

        for (int i = 0; i < length; i++) {
            b[i] = tagBuffer[bPtr++];
        }
        String s = new String(b);
        b = null;
        return s;
    }
    
    /**
     * Reads a length of the data and deposits it into a single Short or an array of Short as needed by the tag's VM.
     * 
     * @return Object
     * 
     * @throws IOException DOCUMENT ME!
     * 
     * @param vm value multiplicity of the DICOM tag data. VM does not represent how many to find.
     * @param length number of bytes to read out of data stream; the length is not used.
     */
    private Object getByte(int vm, int length) throws IOException {
        int len = (elementLength == UNDEFINED_LENGTH) ? 0 : elementLength;
        int i = 0;
        Object readObject = null; // the Object we read in

        if (vm > 1) {
            Byte[] array = new Byte[length];

            while (len > 0) { // we should validate with VM here too
                array[i] = new Byte((byte) getByte());
                len -= 1;
                i++;
            }

            readObject = array;
        } else if ( (vm < 1) && (length > 2)) {

            // not a valid VM, but we don't initialise the VM to 1,
            // so we will use this fact to guess at valid data.
            // we actually do it as above.
            Byte[] array = new Byte[length];

            while (len > 0) { // we should validate with VM here too
                array[i] = new Byte((byte) getByte());
                len -= 1;
                i++;
            }

            readObject = array;
        } else if (length > 0) {
            Byte[] array = new Byte[length];

            while (len > 0) { // we should validate with VM here too
                array[i] = new Byte((byte) getByte());
                len -= 1;
                i++;
            }

            readObject = array;
        }

        return readObject;
    }
    
    /**
     * Reads unsigned bytes from file.
     *
     * @return     The value of unsigned byte read from the file returned as an int.
     *
     * @exception  IOException  if there is an error reading the file
     */
    private final int getByte() throws IOException {
        b3 = 0;

        b3 = (tagBuffer[bPtr] & 0xff);
        bPtr += 1;

        return b3;
    }
    
    /**
     * Reads a length of the data and deposits it into a single Float or an array of Float as needed by the tag's VM.
     * 
     * @return Object
     * 
     * @throws IOException DOCUMENT ME!
     * 
     * @param vm value multiplicity of the DICOM tag data. VM does not represent how many to find.
     * @param length number of bytes to read out of data stream; the length is not used.
     */
    private Object getFloat(int vm, int length) throws IOException {
        int len = (elementLength == UNDEFINED_LENGTH) ? 0 : elementLength;
        int i = 0;
        Object readObject = null;

        if (vm > 1) {
            Float[] array = new Float[length / 4];

            while (len > 0) { // we should validate with VM here too
                array[i] = new Float(getFloat());
                len -= 4;
                i++;
            }

            readObject = array;
        } else if ( (vm < 1) && (length > 4)) {

            // not a valid VM, but we don't initialise the VM to 1,
            // so we will use this fact to guess at valid data
            // we actually do it as above.
            Float[] array = new Float[length / 4];

            while (len > 0) {
                array[i] = new Float(getFloat());
                len -= 4;
                i++;
            }

            readObject = array;
        } else if ( ( (vm == 1) && (length > 4))) {

            // not a valid VM, but we don't initialise the VM to 1,
            // so we will use this fact to guess at valid data.
            // we actually do it as above.
            readObject = new Float(getFloat());
            len -= 4;

            while (len > 0) { // we should validate with VM here too
                getFloat();
                len -= 4;
                i++;
            }
        } else if (length == 4) {
            readObject = new Float(getFloat());
        }

        return readObject;
    }
    
    /**
     * Reads four unsigned bytes from file.
     *
     * @return     The value of the float read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    private final float getFloat() throws IOException {
        int tmpInt;

        if (bigEndian) {
            tmpInt = (((tagBuffer[bPtr] & 0xff) << 24) | ((tagBuffer[bPtr + 1] & 0xff) << 16) |
                          ((tagBuffer[bPtr + 2] & 0xff) << 8) | (tagBuffer[bPtr + 3] & 0xff));
        } else {
            tmpInt = (((tagBuffer[bPtr + 3] & 0xff) << 24) | ((tagBuffer[bPtr + 2] & 0xff) << 16) |
                          ((tagBuffer[bPtr + 1] & 0xff) << 8) | (tagBuffer[bPtr] & 0xff));
        }

        bPtr += 4;

        return (Float.intBitsToFloat(tmpInt));
    }
    
    /**
     * Reads a length of the data and deposits it into a single Double or an array of Double as needed by the tag's VM.
     * 
     * @return Object
     * 
     * @throws IOException DOCUMENT ME!
     * 
     * @param vm value multiplicity of the DICOM tag data. VM does not represent how many to find.
     * @param length number of bytes to read out of data stream; the length is not used.
     */
    private Object getDouble(int vm, int length) throws IOException {
        int len = (elementLength == UNDEFINED_LENGTH) ? 0 : elementLength;
        int i = 0;
        Object readObject = null;

        if (vm > 1) {
            Double[] array = new Double[length / 8];

            while (len > 0) { // we should validate with VM here too
                array[i] = new Double(getDouble());
                len -= 8;
                i++;
            }

            readObject = array;
        } else if ( (vm < 1) && (length > 8)) {

            // not a valid VM, but we don't initialise the VM to 1,
            // so we will use this fact to guess at valid data
            // we actually do it as above.
            Double[] array = new Double[length / 8];

            while (len > 0) {
                array[i] = new Double(getDouble());
                len -= 8;
                i++;
            }

            readObject = array;
        } else if ( ( (vm == 1) && (length > 8))) {

            // not a valid VM, but we don't initialise the VM to 1,
            // so we will use this fact to guess at valid data.
            // we actually do it as above.
            readObject = new Double(getDouble());
            len -= 8;

            while (len > 0) { // we should validate with VM here too
                getDouble();
                len -= 8;
                i++;
            }
        } else if (length == 8) {
            readObject = new Double(getDouble());
        }

        return readObject;
    }
    
    /**
     * Reads eight unsigned bytes from file.
     *
     * @return     The value of the double read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    private final double getDouble() throws IOException {
        b1 = (tagBuffer[bPtr] & 0xff);
        b2 = (tagBuffer[bPtr + 1] & 0xff);
        b3 = (tagBuffer[bPtr + 2] & 0xff);
        b4 = (tagBuffer[bPtr + 3] & 0xff);
        b5 = (tagBuffer[bPtr + 4] & 0xff);
        b6 = (tagBuffer[bPtr + 5] & 0xff);
        b7 = (tagBuffer[bPtr + 6] & 0xff);
        b8 = (tagBuffer[bPtr + 7] & 0xff);

        long tmpLong;

        if (bigEndian) {
            tmpLong = (((long) b1 << 56) | ((long) b2 << 48) | ((long) b3 << 40) | ((long) b4 << 32) |
                           ((long) b5 << 24) | ((long) b6 << 16) | ((long) b7 << 8) | b8);
        } else {
            tmpLong = (((long) b8 << 56) | ((long) b7 << 48) | ((long) b6 << 40) | ((long) b5 << 32) |
                           ((long) b4 << 24) | ((long) b3 << 16) | ((long) b2 << 8) | b1);
        }

        bPtr += 8;

        return (Double.longBitsToDouble(tmpLong));

    }
    
    
    
}
