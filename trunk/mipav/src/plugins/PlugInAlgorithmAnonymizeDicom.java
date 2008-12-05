import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintStream;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.TreeMap;
import java.util.Vector;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.DicomDictionary;
import gov.nih.mipav.model.file.FileBase;
import gov.nih.mipav.model.file.FileDicom;
import gov.nih.mipav.model.file.FileDicomKey;
import gov.nih.mipav.model.file.FileDicomSQ;
import gov.nih.mipav.model.file.FileDicomTag;
import gov.nih.mipav.model.file.FileDicomTagInfo;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileRaw;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;


public class PlugInAlgorithmAnonymizeDicom extends AlgorithmBase {

	// ~ Instance fields ---------------------------------------------------------------------------------------
	/** File selected by the user */	
	private File[] selectedFiles;
	
	/** Additional tag list provided in the dialog */
	private String[] tagListFromDialog;

	private PrintStream printToLogFile;
	
	


	//	~ Constructors -----------------------------------------------------------------------------------------
	public PlugInAlgorithmAnonymizeDicom(File[] inputFiles, String[] tagList) {
		
		selectedFiles = inputFiles;
		tagListFromDialog = tagList;
		
		
		
	}
	

	//  ~ Methods ----------------------------------------------------------------------------------------------
	
	/**
     * Prepares this class for destruction.
     */
    public void finalize() {
        
    	selectedFiles = null;
               
    }
	
	@Override
	public void runAlgorithm() {
		if (selectedFiles == null) {
    		displayError("Selected file is null.");
    		return;
    	}
		int numOfFiles = selectedFiles.length;
		try {
			printToLogFile = new PrintStream(new FileOutputStream(selectedFiles[0].getParent() + File.separator + "AnonymizationResults.txt"));
			printToLogFile.println("Note: The tags listed below were anonymized by the DICOM Anonymization Tool.");
			printToLogFile.println("Private tags and sequence tags are not anonymized but are listed so that the user can anonymize them manually.");
			printToLogFile.println();
		} catch (IOException ioe) {
			ioe.printStackTrace();
		}
		
		for(int i=0; i<numOfFiles; i++) {
			// use the selectedFileName as the reference slice for the file info tag tables
            try {
				printToLogFile.println("Reading next file "+selectedFiles[i].getName()+" with path "+selectedFiles[i].getParent()+File.separator);
            	ReadDicom imageFile = new ReadDicom(selectedFiles[i].getName(), selectedFiles[i].getParent()+File.separator);
	            imageFile.setQuiet(true); // if we want quiet, we tell the reader, too.
	            imageFile.readHeader(true); // can we read the header?
	            printToLogFile.println();
	            imageFile.storeAnonymizeTags();
	            printToLogFile.println();
	            imageFile.storePrivateTags();
	            printToLogFile.println();
	            imageFile.storeSequenceTags();
            } catch(IOException ioe) {
            	ioe.printStackTrace();
            }
		}
		
		System.out.println("Finished reading files");
		printToLogFile.flush();
		printToLogFile.close();
		
	}
	
	private class KeyComparator implements Comparator {

		/* (non-Javadoc)
		 * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
		 */
		@Override
		public int compare(Object o1, Object o2) {
			
			
			if(o1 instanceof FileDicomKey && o2 instanceof FileDicomKey) {
				if(((FileDicomKey) o2).getGroupNumber() == ((FileDicomKey) o1).getGroupNumber()) {
					return ((FileDicomKey) o1).getElementNumber() - ((FileDicomKey)o2).getElementNumber();
				} else {
					return ((FileDicomKey) o1).getGroupNumber() - ((FileDicomKey)o2).getGroupNumber();
				}
			} 
			return o1.hashCode() - o2.hashCode();
			
			
		}
		
	}
	
	private class ReadDicom {

		private HashMap<FileDicomKey, FileDicomSQ> sequenceTags;
		
		private HashMap<FileDicomKey, Object> privateTags;
		
		private HashMap<FileDicomKey, Object> anonymizeTags;

		private String fileName;

		private String fileDir;

		private File fileHeader;

		private RandomAccessFile raFile;

		private FileInfoDicom fileInfo;

		private FileRaw rawFile;

		private int metaGroupLength;

		private int elementLength;

		private int bPtr;

		private int groupWord;

		private int elementWord;

		private boolean encapsulated;

		private boolean lossy;

		private long fLength;

		private boolean hasHeaderBeenRead;
		
		public ReadDicom(String fName, String fDir) throws IOException {
			fileName = fName;
	        fileDir = fDir;

	        try {
	            fileHeader = new File(fileDir + fileName);

	            if (fileHeader == null) {
	                throw new FileNotFoundException();
	            }

	            if (raFile != null) {

	                try {
	                    raFile.close();
	                } catch (IOException ex) {}
	            }

	            try {
	                raFile = new RandomAccessFile(fileHeader, "rw");
	            } catch (IOException e) {
	                raFile = new RandomAccessFile(fileHeader, "r");
	            }

	            fileInfo = new FileInfoDicom(fileName, fileDir, FileUtility.DICOM);
	            fileInfo.setEndianess(LITTLE_ENDIAN);
	            rawFile = new FileRaw(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ);
	        } catch (NullPointerException npe) {
	            npe.printStackTrace();
	        } catch (OutOfMemoryError e) {
	            MipavUtil.displayError("Out of memory in FileDicom constructor.");
	            throw new IOException();
	        }
			
			sequenceTags = new HashMap<FileDicomKey, FileDicomSQ>();
			privateTags = new HashMap<FileDicomKey, Object>();
			anonymizeTags = new HashMap<FileDicomKey, Object>();
		
		}
		
		public void storeSequenceTags() {
			printToLogFile.println("The sequence tags for this file are printed below");
			ArrayList<FileDicomKey> ar;
			Collections.sort(ar = new ArrayList(sequenceTags.keySet()), new KeyComparator());
			Iterator<FileDicomKey> sqItr = ar.iterator();
			while(sqItr.hasNext()) {
				FileDicomKey key = sqItr.next();
				FileDicomSQ sq = sequenceTags.get(key);
				printToLogFile.println("("+key+"):\tBeginning sequence");
            	Vector v = sq.getSequenceDisplay();
            	for(int i=0; i<v.size(); i++) {
            		printToLogFile.println("\t"+v.get(i));
            	}
			}
		}
		
		public void storeAnonymizeTags() {
			printToLogFile.println("The anonymized tags for this file are printed below.");
			printToLogFile.println("Note that private tags can be anonymized by request.");
			ArrayList<FileDicomKey> ar;
			Collections.sort(ar = new ArrayList(anonymizeTags.keySet()), new KeyComparator());
			Iterator<FileDicomKey> prItr = ar.iterator();
			while(prItr.hasNext()) {
				FileDicomKey key = prItr.next();
				Object obj = anonymizeTags.get(key);
				String name = DicomDictionary.getName(key);
				if(name == null) {
					name = "Private Tag";
				}
				printToLogFile.println("("+key+"):\t"+name+"\t"+obj);
			}
		}

		public void storePrivateTags() {
			printToLogFile.println("The private tags for this file are printed below.");
			ArrayList<FileDicomKey> ar;
			Collections.sort(ar = new ArrayList(privateTags.keySet()), new KeyComparator());
			Iterator<FileDicomKey> prItr = ar.iterator();
			while(prItr.hasNext()) {
				FileDicomKey key = prItr.next();
				Object obj = privateTags.get(key);
				printToLogFile.println("("+key+"):\t"+obj);
			}
		}

		/**
	     * Reads in all the tags available in the file and stores them in the Hashtable in FileInfoDicom. This method
	     * handles the various tags that are present at the beginning of a DICOM image file. It also sets the important File
	     * Info variables based on what it finds.
	     * 
	     * <p>
	     * The method will return with a failure code if it finds the tag &quot;0000,0000&quot; or it mis-reads the header
	     * and starts reading from an odd byte.
	     * </p>
	     * 
	     * <p>
	     * As the reader runs through the tags in the header, it reads them based on the type. There are 7 types:
	     * </p>
	     * 
	     * <ul>
	     * <li>typeString</li>
	     * <li>typeShort</li>
	     * <li>typeInt</li>
	     * <li>typeFloat</li>
	     * <li>typeDouble</li>
	     * <li>typeSequence</li>
	     * <li>typeUnknown</li>
	     * </ul>
	     * 
	     * <p>
	     * Any special handling based on type occurs for each tag (@see FileInfoDicom), then it is added to the DICOM tags
	     * table. Each tag is checked against a small list of individual tags, as some tags have an effect on the way the
	     * following tags are interpreted.
	     * </p>
	     * 
	     * <p>
	     * This method also affects some of the properties of the FileInfoDicom.
	     * </p>
	     * 
	     * <ul>
	     * <li>MetaGroupLength</li>
	     * <li>Units of Measure</li>
	     * <li>Transfer Syntax</li>
	     * <li>Extents</li>
	     * <li>Color Pallete for each color channel</li>
	     * </ul>
	     * 
	     * <p>
	     * Display type changes the modality; image length is then also recalculated.
	     * </p>
	     * 
	     * @return <code>true</code> if successful, otherwise <code>false</code>.
	     * 
	     * @exception IOException if there is an error reading the file
	     * 
	     * @see FileInfoDicom
	     * @see #getNextElement(boolean)
	     * @see #convertGroupElement(int, int)
	     */
	    public boolean readHeader(boolean loadTagBuffer) throws IOException {
	        int[] extents;
	        String type, // type of data; there are 7, see FileInfoDicom
	        name; // string representing the tag

	        boolean endianess = FileBase.LITTLE_ENDIAN; // all DICOM files start as little endian (tags 0002)
	        boolean flag = true;

	        if (loadTagBuffer == true) {
	            loadTagBuffer();
	        }

	        String strValue = null;
	        Object data = null;
	        FileDicomSQ sq = null;

	        try {
	            extents = new int[2];
	        } catch (OutOfMemoryError e) {

	            if ( !isQuiet()) {
	                MipavUtil.displayError("Out of memory in FileDicom.readHeader");
	                Preferences.debug("Out of memory in FileDicom.readHeader\n", Preferences.DEBUG_FILEIO);
	            } else {
	                Preferences.debug("Out of memory in FileDicom.readHeader\n", Preferences.DEBUG_FILEIO);
	            }

	            throw new IOException("Out of Memory in FileDicom.readHeader");
	        }

	        metaGroupLength = 0;
	        elementLength = 0;
	        fileInfo.setEndianess(endianess);

	        skipBytes(ID_OFFSET); // Find "DICM" tag

	        // In v. 3.0, within the ID_OFFSET is general header information that
	        // is not encoded into data elements and not present in DICOM v. 2.0.
	        // However, it is optional.

	        if ( !getString(4).equals("DICM")) {
	            fileInfo.containsDICM = false;
	            seek(0); // set file pointer to zero
	        }

	        fileInfo.setDataType(ModelStorageBase.SHORT); // Default file type

	        FileDicomTagTable tagTable = fileInfo.getTagTable();
	        
	        while (flag == true) {

	        	int bPtrOld = bPtr;
	        	boolean isPrivate = false;
	            if (fileInfo.containsDICM) {

	                // endianess is defined in a tag and set here, after the transfer
	                // syntax group has been read in
	                if (getFilePointer() >= (ID_OFFSET + 4 + metaGroupLength)) {
	                    endianess = fileInfo.getEndianess();
	                    // Preferences.debug("endianess = " + endianess + "\n", Preferences.DEBUG_FILEIO);
	                }
	            } else {

	                if (getFilePointer() >= metaGroupLength) {
	                    endianess = fileInfo.getEndianess();
	                }
	            }

	            // ******* Gets the next element
	            getNextElement(endianess); // gets group, element, length
	            name = convertGroupElement(groupWord, elementWord);

	            System.out.println(name);
	            if(name.equals("0008,0018")) {
	            	System.out.println("Stop");
	            }
	            	
	            
	            FileDicomKey key = new FileDicomKey(name);
	            int tagVM;

	            // Preferences.debug("group = " + groupWord + " element = " + elementWord + " length = " +
	            // elementLength + "\n", Preferences.DEBUG_FILEIO);

	            if ( (fileInfo.vr_type == FileInfoDicom.IMPLICIT) || (groupWord == 2)) {

	                // implicit VR means VR is based on tag as defined in dictionary
	                type = DicomDictionary.getType(key);
	                tagVM = DicomDictionary.getVM(key);

	                // the tag was not found in the dictionary..
	                if (type == null) {
	                    type = "typeUnknown";
	                    tagVM = 0;
	                }
	            } else { // Explicit VR
	                //System.err.println("Working with explicit: "+key);
	            	
	            	type = FileDicomTagInfo.getType(new String(vr));

	                if ( !DicomDictionary.containsTag(key)) { //a private tag
	                    tagVM = 0;
	                    isPrivate = true;
	                } else { //not a private tag

	                	FileDicomTagInfo info = (FileDicomTagInfo) DicomDictionary.getInfo(key);
	                    tagVM = info.getValueMultiplicity();
	                }
	            }

	            if ( (elementWord == 0) && (elementLength == 0)) { // End of file

	                if ( !isQuiet()) {
	                    MipavUtil.displayError("Error:  Unexpected end of file: " + fileName + "  Unable to load image.");
	                }

	                throw new IOException("Error while reading header");
	            }

	            if ( (getFilePointer() & 1) != 0) { // The file location pointer is at an odd byte number

	                if ( !isQuiet()) {
	                    MipavUtil.displayError("Error:  Input file corrupted.  Unable to load image.");
	                }

	                // System.err.println("name: "+ name + " , len: " + Integer.toString(elementLength, 0x10));
	                System.err.println("ERROR CAUSED BY READING IMAGE ON ODD BYTE");
	                throw new IOException("Error while reading header");
	            }

	            try {
	            	bPtrOld = bPtr;
	                if (type.equals("typeString")) {
	                    strValue = getString(elementLength);
	                } else if (type.equals("otherByteString")) {

	                    if ( !name.equals(IMAGE_TAG)) {
	                        data = getByte(tagVM, elementLength, endianess);
	                    }
	                } else if (type.equals("otherWordString") && !name.equals("0028,1201") && !name.equals("0028,1202")
	                        && !name.equals("0028,1203")) {

	                    if ( !name.equals(IMAGE_TAG)) {
	                        data = getByte(tagVM, elementLength, endianess);
	                    }
	                } else if (type.equals("typeShort")) {
	                    data = getShort(tagVM, elementLength, endianess);
	                } else if (type.equals("typeInt")) {
	                    data = getInteger(tagVM, elementLength, endianess);
	                } else if (type.equals("typeFloat")) {
	                    data = getFloat(tagVM, elementLength, endianess);
	                } else if (type.equals("typeDouble")) {
	                    data = getDouble(tagVM, elementLength, endianess);
	                }
	                // (type == "typeUnknown" && elementLength == -1) Implicit sequence tag if not in DICOM dictionary.
	                else if (type.equals("typeSequence") || ( (type == "typeUnknown") && (elementLength == -1))) {
	                    int len = elementLength;

	                    // save these values because they'll change as the sequence is read in below.
	                    Preferences
	                            .debug("Sequence Tags: (" + name + "); length = " + len + "\n", Preferences.DEBUG_FILEIO);

	                    sq = (FileDicomSQ)getSequence(endianess, len);
	                    sequenceTags.put(key, sq);
	                    // System.err.print( "SEQUENCE DONE: Sequence Tags: (" + name + "); length = " +
	                    // Integer.toString(len, 0x10) + "\n");

	                    try {
	                    	
	                    } catch (NullPointerException e) {
	                        System.err.println("Null pointer exception while setting value.  Trying to put new tag.");
	                    }
	                    Preferences.debug("Finished sequence tags.\n\n", Preferences.DEBUG_FILEIO);
	                }
	                
	                //check if private
	                if(isPrivate) {
	                	if(type.equals("typeString")) {
		                	privateTags.put(key, strValue);
		                } else if(!type.equals("typeSequence")) {
		                	privateTags.put(key, data);
		                } else {
		                	privateTags.put(key, sq);
		                }
	                }
	                
	                //check if should anonymize, note user can specify private tags to anonymize
	                if(tagExistInAnonymizeTagIDs(key.toString())) {
	   
	                	System.out.print("Writing "+key+"\t");
	                	
	                	long raPtrOld = raFile.getFilePointer();
	                	if(type.equals("typeString")) {
	                		System.out.println(strValue);
		                	anonymizeTags.put(key, strValue);
		                	raFile.seek(bPtrOld);
		                	raFile.writeBytes(strValue);
		                	raFile.seek(raPtrOld);
		                	System.out.println("Writing "+strValue+" to "+bPtrOld+". Returned raPtr to "+raPtrOld);
		                } else if (type.equals("otherByteString")) {

		                    if ( !name.equals(IMAGE_TAG)) {
		                    	System.out.println(data);
		                    	anonymizeTags.put(key, data);
		                    
		                    	byte[] b = new byte[((Byte[])data).length];
		                    	for(int i=0; i<b.length; i++) {
		                    		b[i] = ((Byte[])data)[i];
		                    	}
		                    	raFile.seek(bPtrOld);
			                	raFile.write(b);
			                	raFile.seek(raPtrOld);
		                    }
		                } else if (type.equals("otherWordString") && !name.equals("0028,1201") && !name.equals("0028,1202")
		                        && !name.equals("0028,1203")) {

		                    if ( !name.equals(IMAGE_TAG)) {
		                    	System.out.println(data);
		                    	anonymizeTags.put(key, data);
		                    	
		                    	byte[] b = new byte[((Byte[])data).length];
		                    	for(int i=0; i<b.length; i++) {
		                    		b[i] = ((Byte[])data)[i];
		                    	}
		                    	raFile.seek(bPtrOld);
			                	raFile.write(b);
			                	raFile.seek(raPtrOld);
		                    }
		                } else if (type.equals("typeShort")) {
		                	System.out.println(data);
		                	anonymizeTags.put(key, data);
		                	
		                	raFile.seek(bPtrOld);
		                	if(data instanceof Short) {
		                		writeShort(((Short) data).shortValue(), endianess);
		                	} else if(data instanceof Short[]) {
		                		for(int i=0; i<((Short[])data).length; i++) {
		                			writeShort(((Short[])data)[i], endianess);
		                		}
		                	} else {
		                		System.err.println("Data corruption");
		                	}
		                	raFile.seek(raPtrOld);
		                } else if (type.equals("typeInt")) {
		                	System.out.println(data);
		                	anonymizeTags.put(key, data);
		                	
		                	raFile.seek(bPtrOld);
		                	if(data instanceof Integer) {
		                		writeInt(((Integer) data).intValue(), endianess);
		                	} else if(data instanceof Integer[]) {
		                		for(int i=0; i<((Integer[])data).length; i++) {
		                			writeInt(((Integer[])data)[i], endianess);
		                		}
		                	} else {
		                		System.err.println("Data corruption");
		                	}
		                	raFile.seek(raPtrOld);
		                } else if (type.equals("typeFloat")) {
		                	System.out.println(data);
		                	anonymizeTags.put(key, data);
		                	
		                	raFile.seek(bPtrOld);
		                	if(data instanceof Float) {
		                		writeFloat(((Float) data).floatValue(), endianess);
		                	} else if(data instanceof Integer[]) {
		                		for(int i=0; i<((Float[])data).length; i++) {
		                			writeFloat(((Float[])data)[i], endianess);
		                		}
		                	} else {
		                		System.err.println("Data corruption");
		                	}
		                	raFile.seek(raPtrOld);
		                } else if (type.equals("typeDouble")) {
		                	System.out.println(data);
		                	anonymizeTags.put(key, data);
		                	
		                	raFile.seek(bPtrOld);
		                	if(data instanceof Double) {
		                		writeDouble(((Double) data).doubleValue(), endianess);
		                	} else if(data instanceof Integer[]) {
		                		for(int i=0; i<((Double[])data).length; i++) {
		                			writeDouble(((Double[])data)[i], endianess);
		                		}
		                	} else {
		                		System.err.println("Data corruption");
		                	}
		                	raFile.seek(raPtrOld);
		                }
		                
		              //Note sequence tags are not eligible for anonymization (could easily be changed)
		                //else if(type.equals("typeSequence")) {
		                //	printToLogFile.println("("+key+"):\tBeginning sequence"+elementLength);
	                    //	Vector v = sq.getSequenceDisplay();
	                    //	for(int i=0; i<v.size(); i++) {
	                    //		printToLogFile.println("\t"+v.get(i));
	                    //	}
		                //}
	                }
	                
	                
	                
	            } catch (OutOfMemoryError e) {

	                if ( !isQuiet()) {
	                    MipavUtil.displayError("Out of memory in FileDicom.readHeader");
	                    Preferences.debug("Out of memory in FileDicom.readHeader\n", Preferences.DEBUG_FILEIO);
	                } else {
	                    Preferences.debug("Out of memory in FileDicom.readHeader\n", Preferences.DEBUG_FILEIO);
	                }

	                e.printStackTrace();

	                throw new IOException();
	            }

	            if (name.equals("0002,0000")) { // length of the transfer syntax group

	                if (data != null) {
	                    metaGroupLength = ((Integer) (data)).intValue() + 12; // 12 is the length of 0002,0000 tag
	                }

	                Preferences.debug("metalength = " + metaGroupLength + " location " + getFilePointer() + "\n",
	                        Preferences.DEBUG_FILEIO);
	            } else if (name.equals("0018,602C")) {
	                fileInfo.setUnitsOfMeasure(FileInfoBase.CENTIMETERS, 0);
	            } else if (name.equals("0018,602E")) {
	                fileInfo.setUnitsOfMeasure(FileInfoBase.CENTIMETERS, 1);
	            } else if (name.equals("0002,0010")) {

	                // Transfer Syntax UID: DICOM part 10 page 13, part 5 p. 42-48, Part 6 p. 53
	                // 1.2.840.10008.1.2 Implicit VR Little Endian (Default)
	                // 1.2.840.10008.1.2.1 Explicit VR Little Endian
	                // 1.2.840.10008.1.2.2 Explicit VR Big Endian
	                // 1.2.840.10008.1.2.4.50 8-bit Lossy JPEG (JPEG Coding Process 1)
	                // 1.2.840.10008.1.2.4.51 12-bit Lossy JPEG (JPEG Coding Process 4)
	                // 1.2.840.10008.1.2.4.57 Lossless JPEG Non-hierarchical (JPEG Coding Process 14)
	                // we should bounce out if we don't support this transfer syntax
	                if (strValue.trim().equals("1.2.840.10008.1.2")) {
	                    Preferences.debug("File Dicom: readHeader - Transfer Syntax = " + strValue
	                            + " Implicit VR - Little Endian \n", Preferences.DEBUG_FILEIO);

	                    fileInfo.setEndianess(FileBase.LITTLE_ENDIAN);
	                    fileInfo.vr_type = FileInfoDicom.IMPLICIT;
	                    encapsulated = false;
	                } else if (strValue.trim().equals("1.2.840.10008.1.2.1")) {
	                    Preferences.debug("File Dicom: readHeader - Transfer Syntax = " + strValue
	                            + " Explicit VR - Little Endian \n", Preferences.DEBUG_FILEIO);

	                    fileInfo.setEndianess(FileBase.LITTLE_ENDIAN);
	                    fileInfo.vr_type = FileInfoDicom.EXPLICIT;
	                    encapsulated = false;
	                } else if (strValue.trim().equals("1.2.840.10008.1.2.2")) {
	                    Preferences.debug("File Dicom: readHeader - Transfer Syntax = " + strValue
	                            + " Explicit VR - Big Endian \n", Preferences.DEBUG_FILEIO);

	                    fileInfo.setEndianess(FileBase.BIG_ENDIAN);
	                    fileInfo.vr_type = FileInfoDicom.EXPLICIT;
	                    encapsulated = false;
	                } else if (strValue.trim().startsWith("1.2.840.10008.1.2.4.")) { // JPEG
	                    Preferences.debug("File Dicom: readHeader - Transfer Syntax = " + strValue
	                            + " Implicit VR - Little Endian \n", Preferences.DEBUG_FILEIO);

	                    fileInfo.setEndianess(FileBase.LITTLE_ENDIAN);
	                    fileInfo.vr_type = FileInfoDicom.EXPLICIT;
	                    encapsulated = true;

	                    if (strValue.trim().equals("1.2.840.10008.1.2.4.57")
	                            || strValue.trim().equals("1.2.840.10008.1.2.4.58")
	                            || strValue.trim().equals("1.2.840.10008.1.2.4.65")
	                            || strValue.trim().equals("1.2.840.10008.1.2.4.66")
	                            || strValue.trim().equals("1.2.840.10008.1.2.4.70")) {
	                        lossy = false;
	                    } else {
	                        lossy = true;
	                    }
	                } else {
	                    Preferences.debug("File Dicom: readHeader - Transfer Syntax = " + strValue + " unknown!\n",
	                            Preferences.DEBUG_FILEIO);

	                    if ( !isQuiet()) {
	                        MipavUtil.displayError("MIPAV does not support transfer syntax:\n" + strValue);
	                    }

	                    flag = false; // break loop

	                    return false; // couldn't read it!
	                }
	            } else if (name.equals(IMAGE_TAG)) { // && elementLength!=0) { // The image.
	            	System.out.println("Reading "+name+" image data");
	                // This complicated code determines whether or not the offset is correct for the image.
	                // For that to be true, (height * width * pixel spacing) + the present offset should equal
	                // the length of the file. If not, 4 might need to be added (I don't know why). If not again,
	                // the function returns false.

	                int imageLength = extents[0] * extents[1] * fileInfo.bitsAllocated / 8;

	                Preferences.debug("File Dicom: readHeader - Data tag = " + IMAGE_TAG + "\n", Preferences.DEBUG_FILEIO);
	                Preferences.debug("File Dicom: readHeader - imageLength = " + imageLength + "\n",
	                        Preferences.DEBUG_FILEIO);
	                Preferences.debug("File Dicom: readHeader - getFilePointer = " + getFilePointer() + "\n",
	                        Preferences.DEBUG_FILEIO);

	                if (fileInfo.getModality() == FileInfoBase.POSITRON_EMISSION_TOMOGRAPHY) {
	                    fileInfo.displayType = ModelStorageBase.FLOAT;
	                } else {
	                    fileInfo.displayType = fileInfo.getDataType();
	                }

	                if ( !encapsulated) {
	                    // System.err.println( "\n" +
	                    // Long.toString(getFilePointer()) + " " +
	                    // Long.toString(raFile.length()) +
	                    // " image length = " + imageLength );

	                    long fileEnd;

	                    if (loadTagBuffer == true) {
	                        fileEnd = raFile.length();
	                    } else {
	                        fileEnd = fLength;
	                    }

	                    if ( (imageLength + getFilePointer()) <= fileEnd) {
	                        fileInfo.setOffset((int) getFilePointer()); // Mark where the image is
	                    }
	                    // I think the extra 4 bytes is for explicit tags!!
	                    // see Part 5 page 27 1998
	                    else if ( (imageLength + getFilePointer() + 4) <= fileEnd) {
	                        fileInfo.setOffset((int) getFilePointer() + 4);
	                    } else {

	                        // Preferences.debug( "File Dicom: readHeader: xDim = " + extents[0] + " yDim = " + extents[1] +
	                        // " Bits allocated = " + fileInfo.bitsAllocated, 2);
	                        if ( !isQuiet()) {
	                            MipavUtil.displayError("Image not at expected offset.");
	                        }

	                        throw new IOException("Error while reading header");
	                    }
	                } else { // encapsulated
	                    fileInfo.setOffset((int) (getFilePointer() - 12));
	                }

	                fileInfo.setExtents(extents);
	                flag = false; // break loop
	            } else if (type.equals("typeUnknown")) { // Private tag, may not be reading in correctly.

	                try {

	                    // set the value if the tag is in the dictionary (which means it isn't private..) or has already
	                    // been put into the tag table without a value (private tag with explicit vr)
	                    if (DicomDictionary.containsTag(key) || tagTable.containsTag(key)) {
	                    	printToLogFile.println("Note unknown data ("+key+"):\t"+readUnknownData()+"\t"+elementLength);
	                    	//tagTable.setValue(key, readUnknownData(), elementLength);
	                    } else {
	                    	printToLogFile.println("Note private data ("+key+"):\t"+readUnknownData()+"\t"+elementLength);
	                    	//tagTable
	                        //        .putPrivateTagValue(new FileDicomTagInfo(key, null, tagVM, "PrivateTag", "Private Tag"));

	                        //tagTable.setValue(key, readUnknownData(), elementLength);

	                        Preferences.debug("Group = " + groupWord + " element = " + elementWord + " Type unknown"
	                                + "; value = " + strValue + "; element length = " + elementLength + "\n",
	                                Preferences.DEBUG_FILEIO);
	                    }
	                } catch (OutOfMemoryError e) {

	                    if ( !isQuiet()) {
	                        MipavUtil.displayError("Out of memory error while reading \"" + fileName
	                                + "\".\nThis may not be a DICOM image.");
	                        Preferences.debug("Out of memory storing unknown tags in FileDicom.readHeader\n",
	                                Preferences.DEBUG_FILEIO);
	                    } else {
	                        Preferences.debug("Out of memory storing unknown tags in FileDicom.readHeader\n",
	                                Preferences.DEBUG_FILEIO);
	                    }

	                    e.printStackTrace();

	                    throw new IOException("Out of memory storing unknown tags in FileDicom.readHeader");
	                } catch (NullPointerException npe) {
	                    System.err.println("name: " + name);
	                    System.err.print("no hashtable? ");
	                    System.err.println(tagTable == null);
	                    throw npe;
	                }
	            }
	        }
	        // Done reading tags

	        String photometricInterp = null;

	        if (tagTable.getValue("0028,0004") != null) {
	            fileInfo.photometricInterp = ((String) (tagTable.getValue("0028,0004"))).trim();
	            photometricInterp = fileInfo.photometricInterp.trim();
	        }

	        if (photometricInterp == null) { // Default to MONOCROME2 and hope for the best
	            photometricInterp = new String("MONOCHROME2");
	        }

	        if ( (photometricInterp.equals("MONOCHROME1") || photometricInterp.equals("MONOCHROME2"))
	                && (fileInfo.pixelRepresentation == FileInfoDicom.UNSIGNED_PIXEL_REP) && (fileInfo.bitsAllocated == 8)) {
	            fileInfo.setDataType(ModelStorageBase.UBYTE);
	            fileInfo.displayType = ModelStorageBase.UBYTE;
	            fileInfo.bytesPerPixel = 1;
	        } else if ( (photometricInterp.equals("MONOCHROME1") || photometricInterp.equals("MONOCHROME2"))
	                && (fileInfo.pixelRepresentation == FileInfoDicom.SIGNED_PIXEL_REP) && (fileInfo.bitsAllocated == 8)) {
	            fileInfo.setDataType(ModelStorageBase.BYTE);
	            fileInfo.displayType = ModelStorageBase.BYTE;
	            fileInfo.bytesPerPixel = 1;
	        } else if ( (photometricInterp.equals("MONOCHROME1") || photometricInterp.equals("MONOCHROME2"))
	                && (fileInfo.pixelRepresentation == FileInfoDicom.UNSIGNED_PIXEL_REP) && (fileInfo.bitsAllocated > 8)) {
	            fileInfo.setDataType(ModelStorageBase.USHORT);
	            fileInfo.displayType = ModelStorageBase.USHORT;
	            fileInfo.bytesPerPixel = 2;
	        } else if ( (photometricInterp.equals("MONOCHROME1") || photometricInterp.equals("MONOCHROME2"))
	                && (fileInfo.pixelRepresentation == FileInfoDicom.SIGNED_PIXEL_REP) && (fileInfo.bitsAllocated > 8)) {
	            fileInfo.setDataType(ModelStorageBase.SHORT);
	            fileInfo.displayType = ModelStorageBase.SHORT;
	            fileInfo.bytesPerPixel = 2;
	        }
	        // add something for RGB DICOM images - search on this !!!!
	        else if (photometricInterp.equals("RGB") && (fileInfo.bitsAllocated == 8)) {
	            fileInfo.setDataType(ModelStorageBase.ARGB);
	            fileInfo.displayType = ModelStorageBase.ARGB;
	            fileInfo.bytesPerPixel = 3;

	            if (tagTable.getValue("0028,0006") != null) {
	                fileInfo.planarConfig = ((Short) (tagTable.getValue("0028,0006"))).shortValue();
	            } else {
	                fileInfo.planarConfig = 0; // rgb, rgb, rgb
	            }
	        } else if (photometricInterp.equals("YBR_FULL_422") && (fileInfo.bitsAllocated == 8) && encapsulated) {
	            fileInfo.setDataType(ModelStorageBase.ARGB);
	            fileInfo.displayType = ModelStorageBase.ARGB;
	            fileInfo.bytesPerPixel = 3;

	            if (tagTable.getValue("0028,0006") != null) {
	                fileInfo.planarConfig = ((Short) (tagTable.getValue("0028,0006"))).shortValue();
	            } else {
	                fileInfo.planarConfig = 0; // rgb, rgb, rgb
	            }
	        } else if (photometricInterp.equals("PALETTE COLOR")
	                && (fileInfo.pixelRepresentation == FileInfoDicom.UNSIGNED_PIXEL_REP) && (fileInfo.bitsAllocated == 8)) {
	            fileInfo.setDataType(ModelStorageBase.UBYTE);
	            fileInfo.displayType = ModelStorageBase.UBYTE;
	            fileInfo.bytesPerPixel = 1;

	            int[] dimExtents = new int[2];
	            dimExtents[0] = 4;
	            dimExtents[1] = 256;
	            lut = new ModelLUT(ModelLUT.GRAY, 256, dimExtents);

	            for (int q = 0; q < dimExtents[1]; q++) {
	                lut.set(0, q, 1); // the alpha channel is preloaded.
	            }
	            // sets the LUT to exist; we wait for the pallete tags to
	            // describe what the lut should actually look like.
	        } else {
	            Preferences.debug("File DICOM: readImage() - Unsupported pixel Representation", Preferences.DEBUG_FILEIO);

	            if (raFile != null) {
	                raFile.close();
	            }

	            return false;
	        }

	        if (fileInfo.getModality() == FileInfoBase.POSITRON_EMISSION_TOMOGRAPHY) {
	            fileInfo.displayType = ModelStorageBase.FLOAT;
	            // a bit of a hack - indicates Model image should be reallocated to float for PET image the data is stored
	            // as 2 bytes (short) but is "normalized" using the slope parameter required for PET images (intercept
	            // always 0 for PET).
	        }

	        if ( ( (fileInfo.getDataType() == ModelStorageBase.UBYTE) || (fileInfo.getDataType() == ModelStorageBase.USHORT))
	                && (fileInfo.getRescaleIntercept() < 0)) {
	            // this performs a similar method as the pet adjustment for float images stored on disk as short to read in
	            // signed byte and signed short images stored on disk as unsigned byte or unsigned short with a negative
	            // rescale intercept
	            if (fileInfo.getDataType() == ModelStorageBase.UBYTE) {
	                fileInfo.displayType = ModelStorageBase.BYTE;
	            } else if (fileInfo.getDataType() == ModelStorageBase.USHORT) {
	                fileInfo.displayType = ModelStorageBase.SHORT;
	            }
	        }

	        if (tagTable.getValue("0028,1201") != null) {
	            // red channel LUT
	            FileDicomTag tag = tagTable.get(new FileDicomKey("0028,1201"));
	            Object[] values = tag.getValueList();
	            int lutVals = values.length;

	            // load LUT.
	            if (values instanceof Byte[]) {

	                for (int qq = 0; qq < lutVals; qq++) {
	                    lut.set(1, qq, ((Byte) values[qq]).intValue());
	                }
	            } else {

	                for (int qq = 0; qq < lutVals; qq++) {
	                    lut.set(1, qq, ((Short) values[qq]).intValue());
	                }
	            }
	        }

	        if (tagTable.getValue("0028,1202") != null) {

	            // green channel LUT
	            FileDicomTag tag = tagTable.get(new FileDicomKey("0028,1202"));
	            Object[] values = tag.getValueList();
	            int lutVals = values.length;

	            // load LUT.
	            if (values instanceof Byte[]) {

	                for (int qq = 0; qq < lutVals; qq++) {
	                    lut.set(2, qq, ((Byte) values[qq]).intValue());
	                }
	            } else {

	                for (int qq = 0; qq < lutVals; qq++) {
	                    lut.set(2, qq, ((Short) values[qq]).intValue());
	                }
	            }
	        }

	        if (tagTable.getValue("0028,1203") != null) {

	            // blue channel LUT
	            FileDicomTag tag = tagTable.get(new FileDicomKey("0028,1203"));
	            Object[] values = tag.getValueList();
	            int lutVals = values.length;

	            // load LUT.
	            if (values instanceof Byte[]) {

	                for (int qq = 0; qq < lutVals; qq++) {
	                    lut.set(3, qq, ((Byte) values[qq]).intValue());
	                }
	            } else {

	                for (int qq = 0; qq < lutVals; qq++) {
	                    lut.set(3, qq, ((Short) values[qq]).intValue());
	                }
	            }

	            // here we make the lut indexed because we know that
	            // all the LUT tags are in the LUT.
	            lut.makeIndexedLUT(null);
	        }

	        hasHeaderBeenRead = true;

	        if ( (loadTagBuffer == true) && (raFile != null)) {
	            raFile.close();
	        }

	        return true;
	    }
	    
	    
	    
	    
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
	}
	
	/**
	 * Validator to test accuracy
	 * @param args
	 * @throws IOException
	 */
	public static void main(String[] args) throws IOException {
		File[] f = new File[1];
		f[0] = new File(args[0]);
		String[] s = new String[1];
		s[0] = "";
		
		PlugInAlgorithmAnonymizeDicom p = new PlugInAlgorithmAnonymizeDicom (f, s);
		p.runAlgorithm();
		p.finalize();
		
		//For now inTest and inCopy should be identical, final implementation will have inTest and inCopy identical except for anonymized images
		System.out.println("Reading in "+args[0]);
		DataInputStream inTest = new DataInputStream(new FileInputStream(new File(args[0])));
		System.out.println("Reading in "+args[1]);
		DataInputStream inCopy = new DataInputStream(new FileInputStream(new File(args[1])));
		int maxSize = inTest.available();
		byte[] b = new byte[maxSize], c = new byte[maxSize];
		inTest.readFully(b);
		inCopy.readFully(c);
		System.out.println("Size compare: "+b.length+"\t"+c.length);
		boolean cons = true;
		for(int i=0; i<b.length; i++) {
			if(b[i] != c[i]) {
				System.out.println("Data corruption at "+i);
				cons = false;
			}
		}
		if(cons) {
			System.out.println("Program passed validation.");
		}
		
	}
	

}
