import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Vector;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.DicomDictionary;
import gov.nih.mipav.model.file.FileBase;
import gov.nih.mipav.model.file.FileDicomItem;
import gov.nih.mipav.model.file.FileDicomKey;
import gov.nih.mipav.model.file.FileDicomSQ;
import gov.nih.mipav.model.file.FileDicomTag;
import gov.nih.mipav.model.file.FileDicomTagInfo;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileRaw;
import gov.nih.mipav.model.file.FileRawChunk;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.util.FileUtil;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ProgressBarInterface;


public class PlugInAlgorithmAnonymizeDicom extends AlgorithmBase {

	public static final String ANONYMIZED = "ANONYMIZED :";
	
	// ~ Instance fields ---------------------------------------------------------------------------------------
	/** File selected by the user */	
	private File[] selectedFiles;
	
	/** Additional tag list provided in the dialog */
	private String[] tagListFromDialog;
	
	/** Stream for writing out anonymized/private/sequence tags. */
	private PrintStream printToLogFile;
	
	/**Location of the anonymized data, wouldn't mind making this a static directory in MIPAV preferences that
	 * as a log file*/
	private String anonLoc;

	/**Location for anonymized images to go*/
	private String submitImageLocation;
	
	//	~ Constructors -----------------------------------------------------------------------------------------
	/**
	 * Main constructor, notes works best when inputFiles come from one image set, since output is meant to 
	 * occur in one place.
	 */
	public PlugInAlgorithmAnonymizeDicom(File[] inputFiles, String[] tagList, String submitImageLocation, String submitPatientLocation) {
		ArrayList<File> selectedFilesList = new ArrayList<File>();
		for(int i=0; i<inputFiles.length; i++) {
			if(inputFiles[i].isDirectory()) {
				for(File f : inputFiles[i].listFiles()) {
					selectedFilesList.add(f); 
				}
			} else {
				selectedFilesList.add(inputFiles[i]);
			}
		}
		
		this.submitImageLocation = submitImageLocation;
		this.selectedFiles = inputFiles;
		this.tagListFromDialog = tagList;
		
		File f = new File(submitPatientLocation);
		if(!f.exists()) {
			f.mkdirs();
		}
		
		f = new File(submitImageLocation);
		if(!f.exists()) {
			f.mkdirs();
		}

		if(selectedFiles.length > 0)
			anonLoc = submitPatientLocation + File.separator + "AnonymizationResults.txt";
		else
			anonLoc ="";
		
	}
	

	//  ~ Methods ----------------------------------------------------------------------------------------------

	/**
     * Prepares this class for destruction.
     */
    public void finalize() {
        printToLogFile = null;
    	selectedFiles = null;        
    }
    
    public String getAnonResultsLoc() {
    	return anonLoc;
    }
	
	@Override
	public void runAlgorithm() {
		if (selectedFiles == null) {
    		displayError("Selected file is null.");
    		return;
    	}
		
		
		File[] allTempFiles = constructTemporaryFiles();
		
		try {
			printToLogFile = new PrintStream(new FileOutputStream(anonLoc));
			printToLogFile.println("Note: The tags listed below were anonymized by the DICOM Anonymization Tool.");
			printToLogFile.println("Private tags and sequence tags are not anonymized but are listed so that the user can anonymize them manually.");
			printToLogFile.println();
		} catch (IOException ioe) {
			ioe.printStackTrace();
		}		
		
		anonymizeTemporaryFiles(allTempFiles);
		
		Preferences.debug("Finished reading files");
		printToLogFile.flush();
		printToLogFile.close();
		
		moveTemporaryFiles(allTempFiles);
		
		Preferences.debug("Finished writing files");
		
	}
	
	private File[] constructTemporaryFiles() {
		int numOfFiles = selectedFiles.length;
		File[] allTempFiles = new File[selectedFiles.length];
		File toWrite;
		for(int i=0; i<numOfFiles; i++) {
			try {
				toWrite = File.createTempFile(selectedFiles[i].getName(), ".tmp");
				toWrite.deleteOnExit();
				System.out.println("Created file: "+toWrite.getAbsolutePath());
				BufferedInputStream in = new BufferedInputStream(new FileInputStream(selectedFiles[i]));
				BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream(toWrite));
				int n;
				while((n = in.read()) != -1) {
					out.write(n);
				}
				out.flush();
				out.close();
				in.close();
				allTempFiles[i] = toWrite;
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		
		return allTempFiles;
	}
	
	private void anonymizeTemporaryFiles(File[] allTempFiles) {
		int numOfFiles = selectedFiles.length;
		ArrayList<Integer> filesNotRead = new ArrayList<Integer>();
		int progressNum = minProgressValue;
		HashMap<FileDicomKey, FileDicomSQ> prevSliceSequenceTags = new HashMap<FileDicomKey, FileDicomSQ>();
		HashMap<FileDicomKey, Object> prevSlicePrivateTags = new HashMap<FileDicomKey, Object>();
		HashMap<FileDicomKey, Object> prevSliceAnonymizeTags = new HashMap<FileDicomKey, Object>();
		for(int i=0; i<numOfFiles; i++) {
			// use the selectedFileName as the reference slice for the file info tag tables
            progressNum = minProgressValue + (int)((maxProgressValue-minProgressValue)*(((double)i)/((double)numOfFiles)));
			fireProgressStateChanged(progressNum, null, "Reading file "+i);
			try {
				if(i>0) {
					printToLogFile.println();
					printToLogFile.println();
					printToLogFile.println("***********************************NEXT FILE*******************************************");
					printToLogFile.println();
				}
				printToLogFile.println("Reading "+(i>0 ? "next " : "")+ "file "+selectedFiles[i].getName()); //TODO: path fix from temp file
            	ReadDicom imageFile = new ReadDicom(allTempFiles[i].getName(), allTempFiles[i].getParent()+File.separator);
	            imageFile.setQuiet(true); // if we want quiet, we tell the reader, too.
	            imageFile.readHeader(true); // can we read the header?
	            printToLogFile.println();
	            printToLogFile.println("The "+(i>0 ? "unique " : "")+"anonymized tags for this file are printed below:");
	            prevSliceAnonymizeTags = imageFile.storeAnonymizeTags(prevSliceAnonymizeTags);
	            printToLogFile.println();
	            printToLogFile.println("The "+(i>0 ? "unique " : "")+"private tags for this file are printed below:");
	            prevSlicePrivateTags = imageFile.storePrivateTags(prevSlicePrivateTags);
	            printToLogFile.println();
	            printToLogFile.println("The "+(i>0 ? "unique " : "")+"sequence tags for this file are printed below:");
	            prevSliceSequenceTags = imageFile.storeSequenceTags(prevSliceSequenceTags);
	            printToLogFile.println();
	            printToLogFile.println();
            } catch(Exception e) {
            	e.printStackTrace();
            	filesNotRead.add(i);
            } 
		}
		
		String stringExp = "";
		for(int i=0; i<filesNotRead.size(); i++) {
			stringExp = stringExp + selectedFiles[filesNotRead.get(i)].getAbsolutePath()+"\n";
		}
		if(stringExp.length() > 0) {
			MipavUtil.displayError("The following files could not be anonymized:\n"+stringExp);
		}
	}
	
	private void moveTemporaryFiles(File[] allTempFiles) {
		int numOfFiles = selectedFiles.length;
		for(int i=0; i<numOfFiles; i++) {
			try {		
				BufferedInputStream in = new BufferedInputStream(new FileInputStream(allTempFiles[i]));
				BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream(submitImageLocation+selectedFiles[i].getName()));
				Preferences.debug("Writing anonymized file: "+submitImageLocation+selectedFiles[i].getName());
				int n;
				while((n = in.read()) != -1) {
					out.write(n);
				}
				out.flush();
				out.close();
				in.close();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}
	
	protected boolean tagExistInAnonymizeTagIDs(String tagName) {
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
	
	private class KeyComparator implements Comparator<FileDicomKey> {

		/* (non-Javadoc)
		 * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
		 */
		public int compare(FileDicomKey o1, FileDicomKey o2) {
			if(((FileDicomKey) o2).getGroupNumber() == ((FileDicomKey) o1).getGroupNumber()) {
				return ((FileDicomKey) o1).getElementNumber() - ((FileDicomKey)o2).getElementNumber();
			}
			return ((FileDicomKey) o1).getGroupNumber() - ((FileDicomKey)o2).getGroupNumber();
		}		
	}
	
	private class ReadDicom extends FileDicomInner {
		
		private HashMap<FileDicomKey, FileDicomSQ> sequenceTags;
		
		private HashMap<FileDicomKey, Object> privateTags;
		
		private HashMap<FileDicomKey, Object> anonymizeTags;
		
		public ReadDicom(String fName, String fDir) throws IOException {
			super(fName, fDir);
			
			sequenceTags = new HashMap<FileDicomKey, FileDicomSQ>();
			privateTags = new HashMap<FileDicomKey, Object>();
			anonymizeTags = new HashMap<FileDicomKey, Object>();
		
		}
		
		public HashMap<FileDicomKey, FileDicomSQ> storeSequenceTags(HashMap<FileDicomKey, FileDicomSQ> prevSliceSequenceTags) {
			ArrayList<FileDicomKey> ar;
			String anonymized = ANONYMIZED+"\t", seqPrefix = "";
			Collections.sort(ar = new ArrayList<FileDicomKey>(sequenceTags.keySet()), new KeyComparator());
			Iterator<FileDicomKey> sqItr = ar.iterator();
			while(sqItr.hasNext()) {
				FileDicomKey key = sqItr.next();
				FileDicomSQ sq = sequenceTags.get(key);
				FileDicomSQ sqPrev = prevSliceSequenceTags.get(key);
				Vector<String> v = sq.getSequenceDisplay();
				Vector<String> vPrev = null;
				if(sqPrev != null)
					vPrev = sqPrev.getSequenceDisplay();
				boolean allSame = false;
				Vector<String> differentEntries = new Vector<String>();
				if(vPrev != null && v.size() == vPrev.size()) {
					allSame = true;
					for(int i=0; i<v.size(); i++) {
	            		if(!v.get(i).equals(vPrev.get(i))) {
	            			allSame = false;
	            			differentEntries.add(v.get(i));
	            		}
	            	}
				}
				if(differentEntries.size() > 0) {
					v = differentEntries;
				}
				if(v.size() > 0 && !allSame) {
					seqPrefix = tagExistInAnonymizeTagIDs(new String(key.getGroup()+","+key.getElement())) ? anonymized : "";
					printToLogFile.println(seqPrefix+"("+key+"):\tBeginning sequence");
	            	for(int i=0; i<v.size(); i++) {
	            		String tagCode = v.get(i).substring(1, 10);
	            		String prefix = tagExistInAnonymizeTagIDs(tagCode) ? anonymized : seqPrefix;
	            		printToLogFile.println("\t"+prefix+v.get(i));
	            	}
				}
			}
			return sequenceTags;
		}
		
		public HashMap<FileDicomKey, Object> storeAnonymizeTags(HashMap<FileDicomKey, Object> prevSliceAnonymizeTags) {
			ArrayList<FileDicomKey> ar;
			Collections.sort(ar = new ArrayList<FileDicomKey>(anonymizeTags.keySet()), new KeyComparator());
			Iterator<FileDicomKey> prItr = ar.iterator();
			while(prItr.hasNext()) {
				FileDicomKey key = prItr.next();
				Object obj = anonymizeTags.get(key);
				String name = DicomDictionary.getName(key);
				if(name == null) {
					name = "Private Tag";
				}
				if(prevSliceAnonymizeTags.get(key) == null || 
						(prevSliceAnonymizeTags.get(key) != null && !printObject(prevSliceAnonymizeTags.get(key)).equals(printObject(obj)))) {
					printToLogFile.println("("+key+"):\t"+name+"\t"+printObject(obj));
				}
			}
			return anonymizeTags;
		}

		public HashMap<FileDicomKey, Object> storePrivateTags(HashMap<FileDicomKey, Object> prevSlicePrivateTags) {
			
			ArrayList<FileDicomKey> ar;
			Collections.sort(ar = new ArrayList<FileDicomKey>(privateTags.keySet()), new KeyComparator());
			Iterator<FileDicomKey> prItr = ar.iterator();
			String anonymized = ANONYMIZED+"\t", prefix = "";
			while(prItr.hasNext()) {
				FileDicomKey key = prItr.next();
				Object obj = privateTags.get(key);
				if(prevSlicePrivateTags.get(key) == null || 
						(prevSlicePrivateTags.get(key) != null && !printObject(prevSlicePrivateTags.get(key)).equals(printObject(obj)))) {
					if(obj instanceof Object[]) {
						System.out.println("Is array "+key);
					}
					prefix = tagExistInAnonymizeTagIDs(new String(key.getGroup()+","+key.getElement())) ? anonymized : "";

					printToLogFile.println(prefix+"("+key+"):\t"+printObject(obj));
				}
			}
			return privateTags;
		}
		
		private String printObject(Object obj) {
			if(obj instanceof Object[]) {
				String s = new String();
				for(int i=0; i<((Object[])obj).length; i++) {
					s+= ((Object[])obj)[i].toString()+" ";
				}
				return s;
			} else {
				return obj.toString();
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
	            
	            if(name.equals("2005,140F")) {
	            	System.out.println("Begin debug analysis.");
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

	                    sq = (FileDicomSQ)getSequence(endianess, len, name);
	                    sequenceTags.put(key, sq);
	                    //System.err.print( "SEQUENCE DONE: Sequence Tags: (" + key + ")"+" "+type);
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
	   
	                	//System.out.print("Writing "+key+"\t");
	                	
	                	long raPtrOld = raFile.getFilePointer();
	                	if(type.equals("typeString")) {
	                		
	                		
	                		//System.out.println(strValue);
		                	anonymizeTags.put(key, strValue);
		                	String anonStr = "";
		                	if(key.equals("0008,0014") || key.equals("0008,0018") || 
		                			key.equals("0020,000E") || key.equals("0020, 000D") || key.equals("0020,0010") || key.equals("0020,0052")) {
		                		for(int i=0; i<strValue.length(); i++) {
			                		if(strValue.charAt(i) == '.') {
			                			anonStr = anonStr+".";
			                		} else {
			                			anonStr = anonStr+"1";
			                		}
			                	}
		                	} else {		                	
			                	for(int i=0; i<strValue.length(); i++) {
			                		anonStr = anonStr+"X";
			                	}
		                	}
		                	
		                	raFile.seek(bPtrOld);
		                	raFile.writeBytes(anonStr); //non-anon would be strValue
		                	raFile.seek(raPtrOld);
		                	System.out.println("Writing "+strValue+" to "+bPtrOld+". Returned raPtr to "+raPtrOld);
		                } else if (type.equals("otherByteString")) {

		                    if ( !name.equals(IMAGE_TAG)) {
		                    	//System.out.println(data);
		                    	anonymizeTags.put(key, data);
		                    
		                    	byte[] b = new byte[((Byte[])data).length];
		                    	for(int i=0; i<b.length; i++) {
		                    		b[i] = 0;
		                    	}
		                    	raFile.seek(bPtrOld);
			                	raFile.write(b);
			                	raFile.seek(raPtrOld);
		                    }
		                } else if (type.equals("otherWordString") && !name.equals("0028,1201") && !name.equals("0028,1202")
		                        && !name.equals("0028,1203")) {

		                    if ( !name.equals(IMAGE_TAG)) {
		                    	//System.out.println(data);
		                    	anonymizeTags.put(key, data);
		                    	
		                    	byte[] b = new byte[((Byte[])data).length];
		                    	for(int i=0; i<b.length; i++) {
		                    		b[i] = 0;
		                    	}
		                    	raFile.seek(bPtrOld);
			                	raFile.write(b);
			                	raFile.seek(raPtrOld);
		                    }
		                } else if (type.equals("typeShort")) {
		                	//System.out.println(data);
		                	anonymizeTags.put(key, data);
		                	
		                	raFile.seek(bPtrOld);
		                	if(data instanceof Short || data instanceof Integer) {
		                		writeShort((short)0, endianess);
		                	} else if(data instanceof Short[] || data instanceof Integer[]) {
		                		if(data instanceof Integer[]) {
		                			System.err.println("Unusual data type encountered");
		                		}
		                		for(int i=0; i<((Short[])data).length; i++) {
		                			writeShort((short)0, endianess);
		                		}
		                	} else {
		                		System.err.println("Data corruption");
		                	}
		                	raFile.seek(raPtrOld);
		                } else if (type.equals("typeInt")) {
		                	//System.out.println(data);
		                	anonymizeTags.put(key, data);
		                	
		                	raFile.seek(bPtrOld);
		                	if(data instanceof Integer) {
		                		writeInt(0, endianess);
		                	} else if(data instanceof Integer[]) {
		                		for(int i=0; i<((Integer[])data).length; i++) {
		                			writeInt(0, endianess);
		                		}
		                	} else {
		                		System.err.println("Data corruption");
		                	}
		                	raFile.seek(raPtrOld);
		                } else if (type.equals("typeFloat")) {
		                	//System.out.println(data);
		                	anonymizeTags.put(key, data);
		                	
		                	raFile.seek(bPtrOld);
		                	if(data instanceof Float) {
		                		writeFloat(0, endianess);
		                	} else if(data instanceof Float[] || data instanceof Double[]) {
		                		if(data instanceof Double[]) {
		                			System.err.println("Unusual data type encountered");
		                		}
		                		for(int i=0; i<((Float[])data).length; i++) {
		                			writeFloat(0, endianess);
		                		}
		                	} else {
		                		System.err.println("Data corruption");
		                	}
		                	raFile.seek(raPtrOld);
		                } else if (type.equals("typeDouble")) {
		                	anonymizeTags.put(key, data);
		                	
		                	raFile.seek(bPtrOld);
		                	if(data instanceof Double) {
		                		writeDouble(0, endianess);
		                	} else if(data instanceof Double[]) {
		                		for(int i=0; i<((Double[])data).length; i++) {
		                			writeDouble(0, endianess);
		                		}
		                	} else {
		                		System.err.println("Data corruption");
		                	}
		                	raFile.seek(raPtrOld);
		                } 
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
	                    	Object obj = readUnknownData();
	                    	Preferences.debug("Note unknown data ("+key+"):\t"+obj+"\t"+elementLength);
	                    	
	                    	//tagTable.setValue(key, readUnknownData(), elementLength);
	                    } else {
	                    	Object obj = readUnknownData();
	                    	Preferences.debug("Note private data ("+key+"):\t"+obj+"\t"+elementLength);
	                    	privateTags.put(key, obj);
	                    	
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
	}
	
	


	/**
	 * This class reads and writes DICOM files. The DICOM file format consists of header information marked by tags, with
	 * the pixel data as the last tag. Each tag has a length field that contains the length of the tag, so it is possible to
	 * skip over tags that are unrecognized. This class is entirely based on DICOM version 3.0 published by ACR/NEMA and so
	 * tests for all the tags in the Data Dictionary (DICOM v. 3.0 Part 6). It stores these in a table found in
	 * FileInfoDicom.
	 * 
	 * <p>
	 * The Hashtable is based upon the default DICOM dictionary which contains all possible standard DICOM tags. It also
	 * contains many privatehttp://www.google.com/search?hl=en&client=firefox-a&channel=s&rls=org.mozilla%3Aen-US%3Aofficial&hs=ptT&q=Agatston+method+aortic+calcium&btnG=Search tags that are commented out. If the user wishes a specific private tag to be recognized by this
	 * program, he or she should edit the dictionary file. The tag will then be displayed as any other standard tag would be
	 * displayed. Otherwise, all tags are read in, but if their value representation is unrecognized (only the case with
	 * tags not defined in dictionary file) their value is stored as a string. When FileInfoDicom displays the tag
	 * information, it shows the name of the tag as private and the value as the string. The string may contain valid data
	 * or it may contain junk. There is no way of knowing how to properly read in a private tag without a valid value
	 * representation (VR). So if the user wishes to know private tag information, he or she should specify the proper VR in
	 * the dictionary file and be sure that their file conforms to that VR.
	 * </p>
	 * 
	 * <p>
	 * The methods that need to be ported from FileDicom should be placed here, not in FileDicomBaseInner.
	 * </p>
	 * 
	 * @version 1.0 Aug 1, 1999
	 * @see FileIO
	 * @see FileInfoDicom
	 * @see FileRaw
	 * @see FileRawChunk
	 */
	private class FileDicomInner extends FileDicomBaseInner {

	    // ~ Static fields/initializers
	    // -------------------------------------------------------------------------------------

	    /** The tag marking the start of the image data. */
	    public static final String IMAGE_TAG = "7FE0,0010";

	    /** The tag marking the beginning of a dicom sequence. */
	    public static final String SEQ_ITEM_BEGIN = "FFFE,E000";

	    /** The tag marking the end of a dicom sequence. */
	    public static final String SEQ_ITEM_END = "FFFE,E00D";

	    /** The tag marking the end of an undefined length dicom sequence. */
	    public static final String SEQ_ITEM_UNDEF_END = "FFFE,E0DD";

	    // ~ Instance fields
	    // ------------------------------------------------------------------------------------------------

	    /** Length of the value field of data element. */
	    protected int elementLength;

	    /** Second number (DICOM element in a group) in ordered pair of numbers that uniquely identifies a data element. */
	    protected int elementWord;

	    /**
	     * When dicom image data is 'encapsulated,' it may be in pieces, or 'fragments.' don't know quite why, or if pieces
	     * should be kept together. If in fragments, the image data may span several slices, called a 'frame.'
	     */
	    protected boolean encapsulated = false;

	    /** Directory of the image file. */
	    private String fileDir;

	    /** File object of the image. */
	    private File fileHeader;

	    /** Meta data structure in which to save all the DICOM tags. */
	    protected FileInfoDicom fileInfo;

	    /** Name of the file to be read in. */
	    protected String fileName;

	    /** Location of first element. */
	    private final int FIRST_ELEMENT = 132;

	    /** First number (DICOM group) in ordered pair of numbers that uniquely identifies a data element. */
	    protected int groupWord;

	    /** True if the DICOM image header has been read. */
	    protected boolean hasHeaderBeenRead = false;

	    /** Location of 'DICM'. */
	    protected final int ID_OFFSET = 128;

	    /** Illegal element length. */
	    private final int ILLEGAL_LENGTH = -1;

	    /** Reference to the image read into the application. */
	    private ModelImage image;

	    /** True if in a sequence tag. */
	    private boolean inSQ = false;

	    /** Buffer used when reading in encapsulated JPEG images. */
	    private int[] jpegData = null;

	    /** JPEG compression may be lossy or lossless. */
	    protected boolean lossy = false;

	    /** Reference to the LUT (if one is stored with the image). */
	    protected ModelLUT lut;

	    /**
	     * Number of bytes following this File Meta Element (end of the Value field) up to and including the last File Meta
	     * Element of the Group 2 File Meta Information.
	     * 
	     * <p>
	     * See DICOM3 part 10 pages 12-14 (1988).
	     * </p>
	     */
	    protected int metaGroupLength = 0;

	    /** Name of the sequance tag. */
	    private String nameSQ = "";

	    /**
	     * If the file is <i>quiet</i> no user-interaction is performed. Useful for determining whether or not to display
	     * the MipavUtil.displayError() is to be called. This allows the option of leaving the user-interaction and
	     * notification of an error to occur here or to be handled somewhere else (as in, when an IOException is thrown but
	     * we'd prefer to notify the user once, rather than for each exception.) based on programming preferences or
	     * user-debug preference settings.
	     */
	    private boolean quiet = false;

	    /** DOCUMENT ME! */
	    private FileRaw rawFile;

	    /** Undefined element length. */
	    private final int UNDEFINED_LENGTH = -2;

	    /** Value Representation - see FileInfoDicom. */
	    protected byte[] vr = new byte[2];

	    // ~ Constructors
	    // ---------------------------------------------------------------------------------------------------

	    /**
	     * DICOM reader/writer constructor. Creates the access files and ensures that the files are opened for read-write
	     * access; it will fall back to read-only if read-write access to the image file is not granted. The file info gets
	     * the DICOM dictionary set, as well as the endianess property is set. The image itself is not read.
	     * 
	     * @param fName File name.
	     * @param fDir File directory.
	     * 
	     * @exception IOException if there is an error constructing the files.
	     */
	    public FileDicomInner(String fName, String fDir) throws IOException {
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
	    }

	    // ~ Methods
	    // --------------------------------------------------------------------------------------------------------

	    /**
	     * Closes random access file associated with this object.
	     * 
	     * @throws IOException DOCUMENT ME!
	     */
	    public void close() throws IOException {

	        // System.out.println("FileDICOM.close");
	        this.finalize();
	    }

	    /**
	     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
	     * files and sets other elements to <code>null</code>. Exceptions which occur while this method runs (for
	     * instance, the possibility of getting a <code>IOException</code> when closing the image file) is ignored
	     * quietly.
	     */
	    public void finalize() {
	        fileName = null;
	        fileDir = null;
	        fileHeader = null;

	        fileInfo = null;
	        image = null;
	        vr = null;

	        if (rawFile != null) {

	            try {
	                rawFile.close();
	            } catch (IOException ex) {
	                // closing.. ignore errors
	            }

	            rawFile.finalize();
	        }

	        if (raFile != null) {

	            try {
	                raFile.close();
	            } catch (IOException ex) {
	                // closing.. ignore errors
	            }

	            raFile = null;
	        }

	        rawFile = null;
	        nameSQ = null;
	        jpegData = null;

	        try {
	            super.finalize();
	        } catch (Throwable er) {
	            // ignore errors during memory cleanup..
	        }
	    }

	    /**
	     * Accessor that returns the file info.
	     * 
	     * @return Structure containing the file info.
	     */
	    public final FileInfoBase getFileInfo() {
	        return fileInfo;
	    }

	    /**
	     * returns the ModelLUT, if there, which might be specified by the <code>0028,1201</code>, <code>0028,1202</code>,
	     * <code>0028,1203</code> tags.
	     * 
	     * @return DOCUMENT ME!
	     */
	    public final ModelLUT getLUT() {
	        return lut;
	    }

	    /**
	     * Looks for the DICM _tag_ in the File header. If present, the image is DICOM 3.0 format.
	     * 
	     * @throws IOException Indicates error reading the file
	     * 
	     * @return boolean true if the DICM tag was found in the image header.
	     */
	    public boolean isDICOM() throws IOException {

	        if (raFile == null) {
	            return false;
	        }

	        if (raFile.length() <= ID_OFFSET) {
	            return false;
	        }

	        long fPtr = raFile.getFilePointer();
	        raFile.seek(ID_OFFSET); // Find "DICM" tag

	        // In v. 3.0, within the ID_OFFSET is general header information that
	        // is not encoded into data elements and not present in DICOM v. 2.0.
	        // However, it is optional.

	        if ( !getStringFromFile(4).equals("DICM")) {
	            fileInfo.containsDICM = false;
	            raFile.seek(0); // set file pointer to zero
	        } else {
	            fileInfo.containsDICM = true;
	            raFile.seek(fPtr); // set file pointer back to original position
	        }

	        return (fileInfo.containsDICM);
	    }

	    /**
	     * Dicom version 2: Does not have a preamble in which to search for "DICM" So...the solution will be to search that
	     * it has at least a couple of beginning "08" tags However, we do not know if its Little Endian or Big Endian..so
	     * need to handle both
	     * 
	     * @return
	     * @throws IOException
	     */
	    public boolean isDICOM_ver2() throws IOException {
	        if (raFile == null) {
	            return false;
	        }
	        raFile.seek(0);
	        boolean endianess = FileBase.LITTLE_ENDIAN;

	        //read first byte
	    	byte b = raFile.readByte();
	    	if(b == 8) {
	    		b = raFile.readByte();
	    		if(b == 0) {
	        		//could be dicom 2...need to find at least 1 more 08
	        		//endianess is LITTLE ENDIAN
	        		endianess = FileBase.LITTLE_ENDIAN;
	    		}else{
	    			//definitely not dicom 2....since the first tag is not 08
	    			return false;
	    		}
	    	}else {
	    		if(b == 0) {
		    		b = raFile.readByte();
		    		if(b == 8) {
		    			//could be dicom 2...need to find at least 1 more 08
		    			//endianess is BIG ENDIAN
		    			endianess = FileBase.BIG_ENDIAN;
		    		}else {
		    			//definitely not dicom 2....since the first tag is not 08
		    			return false;
		    		}
	    		}else {
	    			//definitely not dicom 2....since the first tag is not 08
	    			return false;
	    		}
	    	}

	        // skip next 2 bytes
	        raFile.skipBytes(2);

	        // read in length...4 bytes long
	        byte[] byteBuffer = new byte[4];
	        int skipLength;
	        if (endianess == FileBase.LITTLE_ENDIAN) {
	            byteBuffer[3] = raFile.readByte();
	            byteBuffer[2] = raFile.readByte();
	            byteBuffer[1] = raFile.readByte();
	            byteBuffer[0] = raFile.readByte();

	            skipLength = ( (byteBuffer[0] & 0xff) << 24) | ( (byteBuffer[1] & 0xff) << 16)
	                    | ( (byteBuffer[2] & 0xff) << 8) | (byteBuffer[3] & 0xff);
	        } else {
	            byteBuffer[0] = raFile.readByte();
	            byteBuffer[1] = raFile.readByte();
	            byteBuffer[2] = raFile.readByte();
	            byteBuffer[3] = raFile.readByte();

	            skipLength = ( (byteBuffer[0] & 0xff) << 24) | ( (byteBuffer[1] & 0xff) << 16)
	                    | ( (byteBuffer[2] & 0xff) << 8) | (byteBuffer[3] & 0xff);
	        }

	        // skip over the length
	        raFile.skipBytes(skipLength);

	        if (endianess == FileBase.LITTLE_ENDIAN) {
	            b = raFile.readByte();
	            if (b == 8) {
	            	b = raFile.readByte();
	        		if(b == 0) {
	        			// is dicom...since we have at least 2 08 tags
	            		return true;
	        		}else{
	        			//not dicom
	        			return false;
	        		}
	            } else {
	                // not dicom
	                return false;
	            }
	        } else {
	            b = raFile.readByte();
	            if(b == 0) {
	            	b = raFile.readByte();
		            if (b == 8) {
		                // is dicom...since we have at least 2 08 tags
		                return true;
		            } else {
		                // not dicom
		                return false;
		            }
	            }else {
	            	// not dicom
	                return false;
	            }
	        }
	    }

	    /**
	     * gets the quiet option on the class.
	     * 
	     * <p>
	     * The idea is that for all output messages, there should be the option to not bother the user with the message.
	     * Ie., if an exception is thrown, and we normally tell the user that an error occurred, a calling class can set
	     * this option so that the calling class can handle (either loudly or quietly, as needed) the error with its own
	     * message. this could be upgraded to call with quiet to prevent user-queries from interrupting an automatic
	     * process. But that is in the future.
	     * </p>
	     * 
	     * <p>
	     * Note: In the future, this method and variable is to be moved to FileBase.
	     * </p>
	     * 
	     * @return whether this class should consider itself quiet, and by internal inspection, not notify the user. <code>
	     *          True</code>
	     *         is to consider itself to not notify the user. <code>False</code> is to notify the user, and is the
	     *         default behaviour.
	     */
	    public final boolean isQuiet() {
	        return quiet;
	    }

	    public boolean readHeader(boolean loadTagBuffer) throws IOException {
	        //Overwritten by ReadDicom
	    	return false;
	    }

	    public final void setQuiet(boolean q) {
	        quiet = q;
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
	    protected String convertGroupElement(int groupWord, int elementWord) {
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
	     * Reads a length of the data and deposits it into a single Short or an array of Short as needed by the tag's VM.
	     * 
	     * @return Object
	     * 
	     * @throws IOException DOCUMENT ME!
	     * 
	     * @param vm value multiplicity of the DICOM tag data. VM does not represent how many to find.
	     * @param length number of bytes to read out of data stream; the length is not used.
	     * @param endianess byte order indicator; here <code>true</code> indicates big-endian and <code>false</code>
	     *            indicates little-endian.
	     */
	    protected Object getByte(int vm, int length, boolean endianess) throws IOException {
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
	     * Reads a set of DICOM tags from a DICOM sequence item, ending with the data-element end tag <code>
	     * FFFE,E00D</code>.
	     * This list of tags in a DICOM sequence item and places them into a hashtable held within a FileDicomItem.
	     * 
	     * @param itemLength Length of the item in bytes.
	     * @param endianess Big (true) or little (false).
	     * @param sequence name
	     * 
	     * @return The sequence item read in.
	     * 
	     * @see FileDicomItem
	     */
	    private Object getDataSet(int itemLength, boolean endianess, String name) throws IOException {
	        FileDicomItem item = new FileDicomItem();

	        String type;
	        int iValue = 0;
	        float fValue = 0;
	        double dValue = 0;
	        boolean nullEntry = false;

	        if ( (itemLength == UNDEFINED_LENGTH) || (itemLength == ILLEGAL_LENGTH)) {
	            itemLength = Integer.MAX_VALUE;
	        }

	        if (itemLength == 0) {
	            itemLength = elementLength; // use the length of the SQ tag
	        }

	        int startfptr = (int) getFilePointer();

	        // preread the next tag, because we do not want to store the
	        if ( (getFilePointer() - startfptr) <= itemLength) {

	            // Sequence ITEM delimiter tags
	            boolean oldSQ = inSQ;
	            inSQ = true;
	            getNextElement(endianess);
	            nameSQ = convertGroupElement(groupWord, elementWord);
	            
	            // System.err.println("nameSQ " + nameSQ);
	            inSQ = oldSQ;
	        }
	        // Preferences.debug("Item: "+Integer.toString(groupWord, 0x10)+","+
	        // Integer.toString(elementWord, 0x10)+
	        // " for " + Integer.toString(elementLength, 0x10) +
	        // " # readfrom: " + Long.toString(getFilePointer(), 0x10) + "\n");
	  
	        // either there's an "item end" or we've read the entire element length
	        while ( !nameSQ.equals(SEQ_ITEM_END) && ( (getFilePointer() - startfptr) < itemLength)) {
	            // The following is almost exactly the same as the code in readHeader. The main difference is the
	            // information is stored in a hashtable in DicomItem that is initially empty.

	            FileDicomTag entry;

	            if (fileInfo.vr_type == FileInfoDicom.IMPLICIT) {

	                if (DicomDictionary.containsTag(new FileDicomKey(groupWord, elementWord))) {
	                    entry = new FileDicomTag(DicomDictionary.getInfo(new FileDicomKey(groupWord, elementWord)));
	                    type = entry.getType();
	                } else {

	                    // the tag was not found in the dictionary..
	                    entry = new FileDicomTag(new FileDicomTagInfo(new FileDicomKey(groupWord, elementWord), null, 0,
	                            "PrivateTag", "Private tag"));
	                    type = "typeUnknown";
	                }
	            } else {
	                FileDicomTagInfo info;

	                if (DicomDictionary.containsTag(new FileDicomKey(groupWord, elementWord))) {
	                    info = (FileDicomTagInfo) DicomDictionary.getInfo(new FileDicomKey(groupWord, elementWord)).clone();
	                    entry = new FileDicomTag(info);
	                    entry.setValueRepresentation(new String(vr));
	                } else {
	                    info = new FileDicomTagInfo(new FileDicomKey(groupWord, elementWord), new String(vr), 0,
	                            "PrivateTag", "Private tag");
	                    entry = new FileDicomTag(info);
	                }

	                if (fileInfo.isCurrentTagSQ) {
	                    type = "typeSequence";
	                } else {
	                    type = entry.getType();
	                }

	                nullEntry = true;
	            }

	            try {
	            	long bPtrOld = bPtr;
	            	String strValue = new String();
	            	Object data = null;
	                // (type == "typeUnknown" && elementLength == -1) Implicit sequence tag if not in DICOM dictionary.
	                if (type.equals("typeUnknown") && (elementLength != -1)) {
	                    FileDicomTagInfo info = entry.getInfo();

	                    entry = new FileDicomTag(info, readUnknownData());

	                    item.putTag(nameSQ, entry);
	                    item.setLength(nameSQ, elementLength);
	                } else if (type.equals("typeString") || type.equals("otherWordString")) {

	                    if (elementLength == UNDEFINED_LENGTH) {
	                        elementLength = 0;
	                    }

	                    strValue = getString(elementLength);
	                    entry.setValue(strValue, elementLength);
	                    item.putTag(nameSQ, entry);
	                    item.setLength(nameSQ, elementLength);
	                    // Preferences.debug("aaaaaaString Tag: (" + nameSQ + ");\t" + type + "; value = " + strValue + ";
	                    // element length = "+ elementLength + "\n", 2);
	                } else if (type.equals("otherByteString")) {

	                    if ( !nullEntry && (DicomDictionary.getVM(new FileDicomKey(nameSQ)) > 1)) {
	                        entry.setValue(data = readUnknownData(), elementLength);
	                    } else if (elementLength >= 2) {
	                        entry.setValue(data = readUnknownData(), elementLength);
	                    }

	                    entry.setValueRepresentation("OB");
	                    item.putTag(nameSQ, entry);
	                    item.setLength(nameSQ, elementLength);
	                    // Preferences.debug("Tag: (" + nameSQ + ");\t" + type + "; value = " + iValue + "; element length
	                    // = "+ elementLength + "\n", 2);
	                } else if (type.equals("typeShort")) {

	                    if ( !nullEntry && (DicomDictionary.getVM(new FileDicomKey(nameSQ)) > 1)) {
	                        entry.setValue(data = readUnknownData(), elementLength);
	                    } else if (elementLength > 2) {
	                        entry.setValue(data = readUnknownData(), elementLength);
	                    } else {
	                        iValue = getUnsignedShort(endianess);
	                        data = iValue;
	                        entry.setValue(new Short((short) iValue), elementLength);
	                    }

	                    item.putTag(nameSQ, entry);
	                    item.setLength(nameSQ, elementLength);
	                    // Preferences.debug("Tag: (" + nameSQ + ");\t" + type + "; value = " + iValue + "; element length
	                    // = "+ elementLength + "\n", 2);
	                } else if (type.equals("typeInt")) {

	                    if ( !nullEntry && (DicomDictionary.getVM(new FileDicomKey(nameSQ)) > 1)) {
	                        entry.setValue(data = readUnknownData(), elementLength);
	                    } else if (elementLength > 4) {
	                        entry.setValue(data = readUnknownData(), elementLength);
	                    } else {
	                        iValue = getInt(endianess);
	                        data = iValue;
	                        entry.setValue(new Integer(iValue), elementLength);
	                    }

	                    item.putTag(nameSQ, entry);
	                    item.setLength(nameSQ, elementLength);
	                    // Preferences.debug("Tag: (" + nameSQ + ");\t" + type + "; value = " + iValue + "; element length
	                    // = "+ elementLength + "\n", 2);
	                } else if (type.equals("typeFloat")) {

	                    if ( !nullEntry && (DicomDictionary.getVM(new FileDicomKey(nameSQ)) > 1)) {
	                        entry.setValue(data = readUnknownData(), elementLength);
	                    } else if (elementLength > 4) {
	                        entry.setValue(data = readUnknownData(), elementLength);
	                    } else {
	                        fValue = getFloat(endianess);
	                        data = fValue;
	                        entry.setValue(new Float(fValue), elementLength);
	                    }

	                    item.putTag(nameSQ, entry);
	                    item.setLength(nameSQ, elementLength);
	                    // Preferences.debug("Tag: (" + nameSQ + ");\t" + type + "; value = " + fValue + "; element length
	                    // = "+ elementLength + "\n", 2);
	                } else if (type.equals("typeDouble")) {

	                    if ( !nullEntry && (DicomDictionary.getVM(new FileDicomKey(nameSQ)) > 1)) {
	                        entry.setValue(data = readUnknownData(), elementLength);
	                    } else if(elementLength % 8 == 0 && elementLength != 8) {
	                    	Double[] dArr = new Double[elementLength/8];
	                    	for(int i=0; i<dArr.length; i++) 
	                    		dArr[i] = getDouble(endianess);
	                    	entry.setValue(data = dArr, elementLength);
	                    } else if (elementLength > 8) {
	                        entry.setValue(data = readUnknownData(), elementLength);
	                    } else {
	                        dValue = getDouble(endianess);
	                        data = dValue;
	                        entry.setValue(new Double(dValue), elementLength);
	                    }

	                    item.putTag(nameSQ, entry);
	                    item.setLength(nameSQ, elementLength);
	                    // Preferences.debug("Tag: (" + nameSQ + ");\t" + type + "; value = " + dValue + "; element length
	                    // = "+ elementLength + "\n", 2);
	                }
	                // (type == "typeUnknown" && elementLength == -1) Implicit sequence tag if not in DICOM dictionary.
	                else if (type.equals("typeSequence") || ( (type == "typeUnknown") && (elementLength == -1))) {
	                    int len = elementLength;
	                    //name of sub-sequence
	                    String nameTemp = nameSQ;
	                    Object sq2 = getSequence(endianess, len, nameTemp);
	                    entry.setValue(sq2, len);
	                    item.putTag(nameTemp, entry);
	                    item.setLength(nameTemp, len);
	                    // Preferences.debug("Tag: (" + nameSQ + ");\t" + type +
	                    // "; element length = "+ elementLength + "\n", 2);
	                }
	                
	                //System.out.println("At this sequence name currently: "+nameSQ);
	                if(tagExistInAnonymizeTagIDs(nameSQ) || tagExistInAnonymizeTagIDs(name)) {
	                	//System.out.print("Writing "+nameSQ+"\t");
	                	
	                	long raPtrOld = raFile.getFilePointer();
	                	if(type.equals("typeString")) {
	                		System.out.println(strValue);
		                	String anonStr = "";
		                	if(nameSQ.equals("0008,1155")) {
		                		for(int i=0; i<strValue.length(); i++) {
			                		if(strValue.charAt(i) == '.') {
			                			anonStr = anonStr+".";
			                		} else {
			                			anonStr = anonStr+"1";
			                		}
			                	}
		                	} else {
			                	for(int i=0; i<strValue.length(); i++) {
			                		anonStr = anonStr+"X";
			                	}
		                	}
		                	
		                	raFile.seek(bPtrOld);
		                	raFile.writeBytes(anonStr); //non-anon would be strValue
		                	raFile.seek(raPtrOld);
		                	//System.out.println("Writing "+strValue+" to "+bPtrOld+". Returned raPtr to "+raPtrOld);
		                } else if (type.equals("otherByteString")) {

		                    if ( !nameSQ.equals(IMAGE_TAG)) {
		                    	//System.out.println(data);
		                    
		                    	byte[] b = new byte[((Byte[])data).length];
		                    	for(int i=0; i<b.length; i++) {
		                    		b[i] = 0;
		                    	}
		                    	raFile.seek(bPtrOld);
			                	raFile.write(b);
			                	raFile.seek(raPtrOld);
		                    }
		                } else if (type.equals("otherWordString") && !nameSQ.equals("0028,1201") && !nameSQ.equals("0028,1202")
		                        && !nameSQ.equals("0028,1203")) {

		                    if ( !nameSQ.equals(IMAGE_TAG)) {
		                    	//System.out.println(data);
		                    	
		                    	byte[] b = new byte[((Byte[])data).length];
		                    	for(int i=0; i<b.length; i++) {
		                    		b[i] = 0;
		                    	}
		                    	raFile.seek(bPtrOld);
			                	raFile.write(b);
			                	raFile.seek(raPtrOld);
		                    }
		                } else if (type.equals("typeShort")) {
		                	//System.out.println(data);
		                	
		                	raFile.seek(bPtrOld);
		                	if(data instanceof Short || data instanceof Integer) {
		                		writeShort((short)0, endianess);
		                	} else if(data instanceof Short[] || data instanceof Integer[]) {
		                		if(data instanceof Integer[]) {
		                			System.err.println("Unusual data type encountered");
		                		}
		                		for(int i=0; i<((Short[])data).length; i++) {
		                			writeShort((short)0, endianess);
		                		}
		                	} else {
		                		System.err.println("Data corruption");
		                	}
		                	raFile.seek(raPtrOld);
		                } else if (type.equals("typeInt")) {
		                	//System.out.println(data);
		                	
		                	raFile.seek(bPtrOld);
		                	if(data instanceof Integer) {
		                		writeInt(0, endianess);
		                	} else if(data instanceof Integer[]) {
		                		for(int i=0; i<((Integer[])data).length; i++) {
		                			writeInt(0, endianess);
		                		}
		                	} else {
		                		System.err.println("Data corruption");
		                	}
		                	raFile.seek(raPtrOld);
		                } else if (type.equals("typeFloat")) {
		                	//System.out.println(data);
		                	
		                	raFile.seek(bPtrOld);
		                	if(data instanceof Float || data instanceof Double) {
		                		writeFloat(0, endianess);
		                	} else if(data instanceof Float[] || data instanceof Double[]) {
		                		if(data instanceof Double[]) {
		                			System.err.println("Unusual data type encountered");
		                		}
		                		for(int i=0; i<((Float[])data).length; i++) {
		                			writeFloat(0, endianess);
		                		}
		                	} else {
		                		System.err.println("Data corruption");
		                	}
		                	raFile.seek(raPtrOld);
		                } else if (type.equals("typeDouble")) {
		                	//System.out.println(data);
		                	
		                	raFile.seek(bPtrOld);
		                	if(data instanceof Double) {
		                		writeDouble(0, endianess);
		                	} else if(data instanceof Double[]) {
		                		for(int i=0; i<((Double[])data).length; i++) {
		                			writeDouble(0, endianess);
		                		}
		                	} else {
		                		System.err.println("Data corruption");
		                	}
		                	raFile.seek(raPtrOld);
		                }
	                	
	                }
	            } catch (OutOfMemoryError e) {

	                if ( !isQuiet()) {

	                    // Must add back Matt/Dave 11/2003
	                    MipavUtil.displayError("Out of memory in FileDicom.getDataSet");
	                    // yup, done.... Dave, 04-2004
	                } else {
	                    Preferences.debug("Out of memory in FileDicom.getDataSet\n");
	                    // System.err.println("Out of memory in FileDicom.getDataSet");
	                }

	                throw new IOException();
	            }

	            if ( (getFilePointer() - startfptr) < itemLength) {
	                // preread the next tag, because we have yet to see the
	                // end-sequence tag because we don't want to accidently
	                // read the next new-item tag.

	                // except that SQ is undef-len, and def-len items get an
	                // extra tag read out. // 30-jun-2004
	                getNextElement(endianess);
	                nameSQ = convertGroupElement(groupWord, elementWord);

	                // Preferences.debug("Item: "+Integer.toString(groupWord, 0x10)+","+
	                // Integer.toString(elementWord, 0x10)+
	                // " for " + Integer.toString(elementLength, 0x10) +
	                // " # readfrom: " + Long.toString(getFilePointer(), 0x10) + "\n");

	            }
	        }

	        // System.err.println("\t\t\tItem done.");
	        return item;
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
	     * @param endianess byte order indicator; here <code>true</code> indicates big-endian and <code>false</code>
	     *            indicates little-endian.
	     */
	    protected Object getDouble(int vm, int length, boolean endianess) throws IOException {
	        int len = (elementLength == UNDEFINED_LENGTH) ? 0 : elementLength;
	        int i = 0;
	        Object readObject = null;

	        if (vm > 1) {
	            Double[] array = new Double[length / 8];

	            while (len > 0) { // we should validate with VM here too
	                array[i] = new Double(getDouble(endianess));
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
	                array[i] = new Double(getDouble(endianess));
	                len -= 8;
	                i++;
	            }

	            readObject = array;
	        } else if ( ( (vm == 1) && (length > 8))) {

	            // not a valid VM, but we don't initialise the VM to 1,
	            // so we will use this fact to guess at valid data.
	            // we actually do it as above.
	            readObject = new Double(getDouble(endianess));
	            len -= 8;

	            while (len > 0) { // we should validate with VM here too
	                getDouble(endianess);
	                len -= 8;
	                i++;
	            }
	        } else if (length == 8) {
	            readObject = new Double(getDouble(endianess));
	        }

	        return readObject;
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
	     * @param endianess byte order indicator; here <code>true</code> indicates big-endian and <code>false</code>
	     *            indicates little-endian.
	     */
	    protected Object getFloat(int vm, int length, boolean endianess) throws IOException {
	        int len = (elementLength == UNDEFINED_LENGTH) ? 0 : elementLength;
	        int i = 0;
	        Object readObject = null;

	        if (vm > 1) {
	            Float[] array = new Float[length / 4];

	            while (len > 0) { // we should validate with VM here too
	                array[i] = new Float(getFloat(endianess));
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
	                array[i] = new Float(getFloat(endianess));
	                len -= 4;
	                i++;
	            }

	            readObject = array;
	        } else if ( ( (vm == 1) && (length > 4))) {

	            // not a valid VM, but we don't initialise the VM to 1,
	            // so we will use this fact to guess at valid data.
	            // we actually do it as above.
	            readObject = new Float(getFloat(endianess));
	            len -= 4;

	            while (len > 0) { // we should validate with VM here too
	                getFloat(endianess);
	                len -= 4;
	                i++;
	            }
	        } else if (length == 4) {
	            readObject = new Float(getFloat(endianess));
	        }

	        return readObject;
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
	     * @param endianess byte order indicator; here <code>true</code> indicates big-endian and <code>false</code>
	     *            indicates little-endian.
	     */
	    protected Object getInteger(int vm, int length, boolean endianess) throws IOException {
	        int len = (elementLength == UNDEFINED_LENGTH) ? 0 : elementLength;
	        int i = 0;
	        Object readObject = null;

	        if (vm > 1) {
	            Integer[] array = new Integer[length / 4];

	            while (len > 0) { // we should validate with VM here too
	                array[i] = new Integer(getInt(endianess));
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
	                array[i] = new Integer(getInt(endianess));
	                len -= 4;
	                i++;
	            }

	            readObject = array;
	        } else if ( ( (vm == 1) && (length > 4))) {

	            // not a valid VM, but we don't initialise the VM to 1,
	            // so we will use this fact to guess at valid data.
	            // we actually do it as above.
	            readObject = new Integer(getInt(endianess));
	            len -= 4;

	            while (len > 0) { // we should validate with VM here too
	                getInt(endianess);
	                len -= 4;
	                i++;
	            }
	        } else if (length == 4) {
	            readObject = new Integer(getInt(endianess));
	        }

	        return readObject;
	    }

	    /**
	     * Reads in four integers, then tests for implicit Value Representation(VR) or explicit VR. If explicit, it finds
	     * out what the VR is and stores it.
	     * 
	     * <p>
	     * See DICOM Specification Part 5 (1998) Section 7 pages 24-34.
	     * </p>
	     * 
	     * @param endianess Big or little.
	     * @param b1 First byte of the tag to be tested before applying endianess.
	     * @param b2 Second byte of the tag to be tested before applying endianess.
	     * @param b3 Third byte of the tag to be tested before applying endianess.
	     * @param b4 Fourth byte of the tag to be tested before applying endianess.
	     * 
	     * @return Length of the element.
	     * 
	     * @throws IOException DOCUMENT ME!
	     */
	    private int getLength(boolean endianess, byte b1, byte b2, byte b3, byte b4) throws IOException {
	        boolean implicit = false;

	        if ( (fileInfo.vr_type == FileInfoDicom.IMPLICIT) || (groupWord == 2)) {

	            if (fileInfo.containsDICM) {

	                // at this point transfer syntax not read; we know endianess
	                // is little endian but vr may be explicit
	                if ( (getFilePointer() <= (FIRST_ELEMENT + metaGroupLength)) || (groupWord == 2)) {

	                    if ( ( (b1 < 65) || (b1 > 90)) && ( (b2 < 65) || (b2 > 90))) {
	                        implicit = true;
	                    } else {
	                        implicit = false;
	                    }
	                } else {
	                    implicit = true; // transfer syntax has been read, implicit set
	                }
	            } else {

	                // at this point transfer syntax not read; we know endianess
	                // is little endian but vr may be explicit
	                if ( (getFilePointer() <= metaGroupLength) || (groupWord == 2)) {

	                    if ( ( (b1 < 65) || (b1 > 90)) && ( (b2 < 65) || (b2 > 90))) {
	                        implicit = true;
	                    } else {
	                        implicit = false;
	                    }
	                } else {
	                    implicit = true; // transfer syntax has been read, implicit set
	                }
	            }
	        }

	        // displays the individual bytes. It could be better, for instance
	        // printing as individual integer values:
	        // System.err.print("[ "+Integer.toString(b1, 0x10)+" " +
	        // Integer.toString(b2, 0x10)+" " +
	        // Integer.toString(b3, 0x10)+" " +
	        // Integer.toString(b4, 0x10)+" ]"
	        // );

	        if (implicit) {

	            // implicit VR with 32-bit length
	            if (endianess == FileBase.LITTLE_ENDIAN) {
	                return ( (b1 & 0xff) + ( (b2 & 0xff) << 8) + ( (b3 & 0xff) << 16) + ( (b4 & 0xff) << 24));
	            } else {
	                return ( ( (b1 & 0xff) << 24) + ( (b2 & 0xff) << 16) + ( (b3 & 0xff) << 8) + (b4 & 0xff));
	            }
	        }
	        // explicit VR with 32-bit length
	        else if ( ( (b1 == 79) && (b2 == 66)) || ( (b1 == 79) && (b2 == 87)) || ( (b1 == 83) && (b2 == 81))
	                || ( (b1 == 85) && (b2 == 78)) || ( (b1 == 85) && (b2 == 84))) {

	            // VR = 'OB', or 'OW' or 'SQ' or 'UN' or 'UT'
	            vr[0] = (byte) b1;
	            vr[1] = (byte) b2;
	            fileInfo.isCurrentTagSQ = new String(vr).equals("SQ");

	            // SQ - check for length FFFFFFFF (undefined), otherwise should be 0.
	            if ( (b1 == 83) && (b2 == 81)) { // 'SQ'

	                // but i can't figure out why we're making a big deal out
	                // of SQ types; UNDEF_LENGTH -is- -1 which -is- FF FF FF FF.
	                // maybe ensuring return type?
	                read(byteBuffer4); // reads 4 byte length w/o endianess for testing

	                if ( (byteBuffer4[0] == 255) && (byteBuffer4[1] == 255) && (byteBuffer4[2] == 255)
	                        && (byteBuffer4[3] == 255)) {
	                    return UNDEFINED_LENGTH;
	                } else {
	                    seek(getFilePointer() - 0x4);

	                    return getInt(endianess); // rereads length using proper endianess
	                }
	            } else {
	                return getInt(endianess);
	            }
	        }
	        // explicit VR with 16-bit length
	        else {
	            vr[0] = (byte) b1; // these are not VR for item tags!!!!!!!!!
	            vr[1] = (byte) b2;

	            fileInfo.isCurrentTagSQ = new String(vr).equals("SQ");

	            if (endianess == FileBase.LITTLE_ENDIAN) {
	                return ( (b3 & 0xff) | ( (b4 & 0xff) << 8));
	            } else {
	                return ( ( (b3 & 0xff) << 8) | (b4 & 0xff));
	            }
	        }
	    }

	    /**
	     * Increments the location, then reads the elementWord, groupWord, and elementLength. It also tests for an end of
	     * file and resets the elementWord if it encounters one.
	     * 
	     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
	     *            endian.
	     * 
	     * @throws IOException DOCUMENT ME!
	     */
	    protected void getNextElement(boolean bigEndian) throws IOException {

	        groupWord = getUnsignedShort(bigEndian);
	        elementWord = getUnsignedShort(bigEndian);
	        // Preferences.debug("(just found: )"+Integer.toString(groupWord, 0x10) + ":"+Integer.toString(elementWord,
	        // 0x10)+" - " ); System.err.print("( just found: ) "+ Integer.toString(groupWord, 0x10) +
	        // ":"+Integer.toString(elementWord, 0x10)+ " - ");

	        if (fileInfo.vr_type == FileInfoDicom.EXPLICIT) {

	            /*
	             * explicit tags carry an extra 4 bytes after the tag (group, element) information to describe the type of
	             * tag. the element dictionary describes this info, so we skip past it here. (apr 2004)
	             */
	            String tagname = convertGroupElement(groupWord, elementWord);

	            if (tagname.equals(SEQ_ITEM_BEGIN) || tagname.equals(SEQ_ITEM_END) || tagname.equals(SEQ_ITEM_UNDEF_END)
	                    || tagname.equals("FFFE,EEEE")) // reserved
	            {
	                elementLength = getInt(bigEndian);
	            } else {
	                read(byteBuffer4); // Reads the explicit VR and following two bytes.
	                elementLength = getLength(bigEndian, byteBuffer4[0], byteBuffer4[1], byteBuffer4[2], byteBuffer4[3]);
	                // Preferences.debug(" length " + Integer.toString(elementLength, 0x10) + "\n");
	            }
	        } else { // this is what is standardly used.

	            // either IMPLICIT or group element is not SEQ_ITEM_BEGIN
	            read(byteBuffer4);
	            elementLength = getLength(bigEndian, byteBuffer4[0], byteBuffer4[1], byteBuffer4[2], byteBuffer4[3]);
	        }
	    }

	    /**
	     * Gets the sequence in a sequence tag. Sequences of items have special encodings that are detailed in the DICOM
	     * standard. There is usually an "item" tag, then a dataset encoded exactly like other tags, then a tag indicating
	     * the end of the sequence.
	     * 
	     * <P>
	     * For further information see the DICOM Standard, Part 5, Section 7.
	     * </P>
	     * 
	     * @param endianess Big or little
	     * @param seqLength Length of this sequence, although possibly left undefined.
	     * @param name sequence name
	     * 
	     * @return A DicomSQ object which stores the new tags and their info
	     * 
	     * @see DicomSQ
	     */
	    protected Object getSequence(boolean endianess, int seqLength, String name) throws IOException {
	        FileDicomSQ sq = new FileDicomSQ();
	        
	        // There is no more of the tag to read if the length of the tag
	        // is zero. In fact, trying to get the Next element is potentially
	        // bad, so we'll just shut down the reading here.
	        if (seqLength == 0) {
	            return sq;
	        }

	        // hold on to where the sequence is before items for measuring
	        // distance from beginning of sequence
	        int seqStart = (int) getFilePointer();
	        inSQ = true;

	        getNextElement(endianess); // gets the first ITEM tag
	        Preferences.debug("Item: " + Integer.toString(groupWord, 0x10) + "," + Integer.toString(elementWord, 0x10)
	                + " for " + Integer.toString(elementLength, 0x10) + " # readfrom: "
	                + Long.toString(getFilePointer(), 0x10) + "\n");

	        inSQ = false;
	        nameSQ = convertGroupElement(groupWord, elementWord);

	        // Preferences.debug("getSquence: nameSQ = " + nameSQ +
	        // " fptr = " + Long.toString(getFilePointer(), 0x10) + "\n");
	        try {

	            if ( (seqLength == UNDEFINED_LENGTH) || (seqLength == ILLEGAL_LENGTH)) {

	                while ( !nameSQ.equals(SEQ_ITEM_UNDEF_END)) {

	                    if (nameSQ.equals(SEQ_ITEM_BEGIN)) {

	                        // elementLength here is the length of the
	                        // item as it written into the File
	                        if (elementLength == 0) {
	                            FileDicomItem item = new FileDicomItem();
	                            sq.addItem(item);
	                        } else {
	                            sq.addItem((FileDicomItem) getDataSet(elementLength, endianess, name));
	                        }
	                    } else if (nameSQ.equals(SEQ_ITEM_END)) {

	                        // possibility of getting here when subsequence tag length == -1
	                        // end of sub-sequence tag
	                        Preferences.debug("End of sub-sequence " + SEQ_ITEM_END + " found; nothing done.\n");
	                    } else { // should never get here
	                        Preferences.debug(
	                                "getSequence(): sub-sequence tags not starting with " + SEQ_ITEM_BEGIN + "\n", 2);
	                    }

	                    inSQ = true; // don't add the element length to the location
	                    getNextElement(endianess); // skipping the tag-length???
	                    nameSQ = convertGroupElement(groupWord, elementWord);
	                    inSQ = false; // may now add element length to location

	                    // Preferences.debug("Next item of seq. "+
	                    // "nameSQ: " + nameSQ +
	                    // " fpr: " + Long.toString(getFilePointer(), 0x10) + " \n");
	                }
	            } else { // sequence length is explicitly defined:

	                // MUST be "<", rather than "<=" because when
	                // fptr - seqStart == seqLength we want to move along to
	                // other tags; it indicates that we are done with this
	                // sequence.

	                while ( (getFilePointer() - seqStart) < seqLength) {

	                    // loop is meant to read out each sub-sequence tag from the sequence.
	                    // Must make it able to read out the
	                    Preferences.debug(nameSQ + "; len:" + Long.toString(getFilePointer() - seqStart, 0x10) + "\n");

	                    if (nameSQ.equals(SEQ_ITEM_BEGIN)) { // this should always be true

	                        // int offset = (int)raFile.getFilePointer(); // where the data set begins for this item
	                        // elementLength here is the length of the
	                        // item as it written into the File
	                        // System.out.println("Special ele length = " + Integer.toString(elementLength, 0x10));
	                        sq.addItem((FileDicomItem) getDataSet(elementLength, endianess, name));
	                    } else { // should never get here
	                        Preferences.debug(
	                                "getSequence(): sub-sequence tags not starting with " + SEQ_ITEM_BEGIN + "\n", 2);
	                        // System.err.println("getSequence(): sub-sequence tags not starting with FFFE,E000");
	                    }

	                    if ( (getFilePointer() - seqStart) < seqLength) { // '<=' or just '<'??
	                        inSQ = true; // don't add the element length to the location
	                        Preferences.debug("[FileDicom.getSequence():]" + Integer.toString(groupWord, 0x10) + "-"
	                                + Integer.toString(elementWord, 0x10) + " -- ");
	                        getNextElement(endianess); // skipping the tag-length???
	                        Preferences.debug("  the length: " + Integer.toString(elementLength, 0x10) + "\n");
	                        nameSQ = convertGroupElement(groupWord, elementWord);
	                        
	                        Preferences.debug("after converting group-element: " + nameSQ + "!!!!\n");
	                        inSQ = false; // may now add element length to location

	                        // System.err.println("pulled out next item of seq. nameSQ: " +nameSQ);
	                    }
	                }
	            }
	        } catch (Exception error) {
	            error.printStackTrace();
	            Preferences.debug("Exception caught; Problem in FileDicom.getSequence\n", 2);
	            Preferences.debug(error.toString() + "\n");
	        }

	        return sq;
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
	     * @param endianess byte order indicator; here <code>true</code> indicates big-endian and <code>false</code>
	     *            indicates little-endian.
	     */
	    protected Object getShort(int vm, int length, boolean endianess) throws IOException {
	        int len = (elementLength == UNDEFINED_LENGTH) ? 0 : elementLength;
	        int i = 0;
	        Object readObject = null; // the Object we read in

	        if (vm > 1) {
	            Short[] array = new Short[length / 2];

	            while (len > 0) { // we should validate with VM here too
	                array[i] = new Short((short) getUnsignedShort(endianess));
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
	                array[i] = new Short((short) getUnsignedShort(endianess));
	                len -= 2;
	                i++;
	            }

	            readObject = array;
	        } else if ( ( (vm == 1) && (length > 2))) {

	            // not a valid VM, but we don't initialise the VM to 1,
	            // so we will use this fact to guess at valid data.
	            // we actually do it as above.
	            readObject = new Short((short) getUnsignedShort(endianess));
	            len -= 2;

	            while (len > 0) { // we should validate with VM here too
	                getUnsignedShort(endianess);
	                len -= 2;
	                i++;
	            }
	        } else if (length == 2) {
	            readObject = new Short((short) getUnsignedShort(endianess));
	        }

	        return readObject;
	    }

	    /**
	     * Gets private tags or other tags where the type is unknown; does not change the data, so it may be written out
	     * correctly.
	     * 
	     * @return A Byte array of length elementLength with the data stored in it.
	     * 
	     * @throws IOException DOCUMENT ME!
	     */
	    protected Object readUnknownData() throws IOException {
	        byte[] bytesValue;
	        Byte[] bytesV;
	        Preferences.debug("Unknown data; length is " + elementLength + " fp = " + getFilePointer() + "\n", 2);

	        if (elementLength <= 0) {
	            Preferences.debug("Unknown data; Error length is " + elementLength + "!!!!!\n", 2);

	            return null;
	        }

	        bytesValue = new byte[elementLength];
	        read(bytesValue);
	        bytesV = new Byte[elementLength];

	        for (int k = 0; k < bytesValue.length; k++) {
	            bytesV[k] = new Byte(bytesValue[k]);
	        }

	        return bytesV;
	    }
	    
	    /**
	     * Writes out a sequence tag and its data. The routine writes the Sequence as undefined-length and each of the items
	     * as undefined length. The appropriate write-methods are used to output the various tags to the file.
	     * 
	     * @param outputFile File to write to.
	     * @param vr_type VR type, explicit or implicit
	     * @param sq Sequence to write out
	     * @param endianess Big is <code>true</code> or <code>false</code> for little endian byte-order.
	     * 
	     * @throws IOException if write fails
	     */
	    protected void writeSequence(RandomAccessFile outputFile, boolean vr_type, FileDicomSQ sq, boolean endianess)
	            throws IOException {
	        FileDicomItem item;

	        if ( (sq == null) || (sq.getSequenceLength() < 1)) {
	            return;
	        }

	        for (int i = 0; i < sq.getSequenceLength(); i++) {
	            item = sq.getItem(i);

	            if (item.getNumberOfElements() > 0) {

	                // write item-start tag
	                writeShort((short) 0xFFFE, endianess);
	                writeShort((short) 0xE000, endianess);
	                writeInt(0xFFFFFFFF, endianess); // data-length (we'll get it to spit out real length later!)
	            }

	            Iterator<FileDicomTag> dataSetItr = item.getDataSet().values().iterator();

	            while(dataSetItr.hasNext()) {

	                // System.err.println(" Counter = " + j);
	                String type = "";
	                FileDicomTag entry = dataSetItr.next();

	                String vr = entry.getValueRepresentation();
	                int length = entry.getLength();

	                // System.out.println("adadadfad length = " + length + " group = " + entry.getGroup() + " element = " +
	                // entry.getElement());
	                try {

	                    if ( (vr_type == FileInfoDicom.EXPLICIT) && (vr != null)) {
	                        type = FileDicomTagInfo.getType(vr);
	                    } else {
	                        type = entry.getType();
	                    }
	                } catch (NullPointerException error) {
	                    type = "typeUnknown";
	                }

	                writeShort((short) entry.getGroup(), endianess);
	                writeShort((short) entry.getElement(), endianess);

	                if ( (vr_type == FileInfoDicom.EXPLICIT) && (vr != null)) {
	                    outputFile.writeBytes(vr);

	                    if (vr.equals("SQ") || vr.equals("OB") || vr.equals("OW") || vr.equals("UN")) {

	                        // if (vr.equals("SQ") || vr.equals("UN")) {
	                        outputFile.writeShort(0);
	                    } else {
	                        writeShort((short) length, endianess);
	                    }
	                }

	                if (length == UNDEFINED_LENGTH) {
	                    writeShort((short) 0xFFFF, endianess);
	                } else if ( ( (vr_type == FileInfoDicom.EXPLICIT) && (vr != null) && (vr.equals("SQ")
	                        || vr.equals("OB") || vr.equals("OW") || vr.equals("UN")))) {

	                    if ( (length == 0) && vr.equals("SQ")) {
	                        // Do nothing because we only write ____Dave need help___ sequence tags
	                    } else {
	                        writeInt(length, endianess);
	                    }
	                } else if ( (vr_type == FileInfoDicom.IMPLICIT) && !type.equals("typeUnknown")
	                        && !type.equals("typeSequence")) {
	                    writeInt(length, endianess);
	                }

	                if (type.equals("typeString") || type.equals("otherWordString")) {
	                    String str = new String();
	                    for(int k=0; k<entry.getValue(false).toString().length(); k++) {
	                    	str += "X";
	                    }
	                	
	                	outputFile.writeBytes(str);

	                    if ( (entry.getValue(false).toString().length() % 2) != 0) {
	                        outputFile.writeBytes("\0");
	                    }
	                } else if (type.equals("typeUnknown")) {

	                    // Unknowns are stored as an array of Bytes.
	                    // VM does not apply?
	                    Byte[] bytesV = null;
	                    bytesV = (Byte[]) entry.getValue(false);

	                    byte[] bytesValue = new byte[bytesV.length];

	                    for (int k = 0; k < bytesV.length; k++) {
	                        bytesValue[k] = (byte)0;
	                        // System.err.print(" [" + bytesV[k].toString()+"]");
	                    }

	                    writeInt(bytesV.length, endianess);
	                    outputFile.write(bytesValue);
	                } else if (type.equals("otherByteString")) {
	                    Byte[] bytesV = null;
	                    bytesV = (Byte[]) entry.getValue(false);

	                    byte[] bytesValue = new byte[bytesV.length];

	                    for (int k = 0; k < bytesV.length; k++) {
	                        bytesValue[k] = (byte)0;
	                        // System.err.print(" [" + bytesV[k].toString()+"]");
	                    }

	                    // writeInt(bytesV.length, endianess);
	                    outputFile.write(bytesValue);
	                } else if (type.equals("typeFloat")) {
	                    writeFloat( (new Float(0)).floatValue(), endianess);
	                } else if (type.equals("typeDouble")) {
	                	if(entry.getValue(false) instanceof Double[]) {
	                		Double[] dArr = (Double[]) entry.getValue(false);
	                		for(int k=0; k<dArr.length; k++) {
	                			writeDouble((double)0, endianess);
	                		}
	                	} else
	                		writeDouble( (double)0, endianess);
	                } else if (type.equals("typeShort")) {
	                    writeShort( (short)0, endianess);
	                } else if (type.equals("typeInt")) {
	                    writeInt( (int)0, endianess);
	                } else if (type.equals("typeSequence")) { //sequences within sequences will be anonymized
	                    FileDicomSQ sq2 = (FileDicomSQ) entry.getValue(false);
	                    writeInt(0xFFFFFFFF, endianess);
	                    writeSequence(outputFile, vr_type, sq2, endianess);
	                }
	            }
	          
	            // write end-item tag:
	            writeShort((short) 0xFFFE, endianess);
	            writeShort((short) 0xE00D, endianess);
	            writeInt((int) 0, endianess);
	        }

	        writeShort((short) 0xFFFE, endianess);
	        writeShort((short) 0xE0DD, endianess);
	        writeInt((int) 0, endianess);
	        // System.err.println("3333pointer = " + outputFile.getFilePointer());
	    }
	}

	


	/**
	 * FileDICOMBase is an class that supports the reading/writing of DICOM files. It reads in a buffer of tags that can be
	 * parsed more quickly than continued random accesses to the harddrive.
	 *
	 * @version  1.0 June 30, 2005
	 */
	private class FileDicomBaseInner {

	    //~ Static fields/initializers -------------------------------------------------------------------------------------

	    /** Byte order. Rightmost byte is most significant. */
	    public static final boolean LITTLE_ENDIAN = false;

	    /** Byte order. Leftmost byte is most significant. */
	    public static final boolean BIG_ENDIAN = true;

	    /** Read only access. */
	    public static final int READ = 0;

	    /** Read-write access. */
	    public static final int READ_WRITE = 1;

	    /** The size of the buffer that contains the tags of the DICOM image. Default = 400K. */
	    public static final int BUFFER_SIZE = 400000;

	    //~ Instance fields ------------------------------------------------------------------------------------------------

	    /** Two byte array used to read/write in data so that one doesn't't need to be allocated with each read/write. */
	    protected byte[] byteBuffer2 = new byte[2];

	    /** Four byte array used to read/write in data so that one doesn't need to be allocated with each read/write. */
	    protected byte[] byteBuffer4 = new byte[4];

	    /** Eight byte array used to read/write in data so that they don't need to be allocated with each read/write. */
	    protected byte[] byteBuffer8 = new byte[8];

	    /** Total length of the image file. */
	    protected long fLength = 0;

	    /** Flag indicating if the progress bar should be shown. */
	    protected boolean pBarVisible = true;

	    /** Progress bar to show when reading in image file. */
	    protected ProgressBarInterface progressBar;

	    /** Pointer to file to read or write from. */
	    protected RandomAccessFile raFile;

	    /** The buffer that holds the tags of the DICOM image. */
	    protected byte[] tagBuffer = null;


	    /** Integer variable used to read/write in data so that they don't need to be allocated with each read/write. */
	    private int b1, b2, b3, b4, b5, b6, b7, b8;

	    /** Buffer pointer (aka file pointer). */
	    protected int bPtr = 0;

	    //~ Constructors ---------------------------------------------------------------------------------------------------

	    /**
	     * Empty constructor.
	     */
	    public FileDicomBaseInner() { }

	    //~ Methods --------------------------------------------------------------------------------------------------------

	    /**
	     * Prepares this class for cleanup.
	     */
	    public void finalize() {
	        byteBuffer2 = null;
	        byteBuffer4 = null;
	        byteBuffer8 = null;
	        progressBar = null;
	        tagBuffer = null;

	        if (raFile != null) {

	            try {
	                raFile.close();
	            } catch (IOException ioe) {
	                // Do nothing
	            }
	        }

	        raFile = null;
	    }


	    /**
	     * Reads unsigned bytes from file.
	     *
	     * @return     The value of unsigned byte read from the file returned as an int.
	     *
	     * @exception  IOException  if there is an error reading the file
	     */
	    public final int getByte() throws IOException {
	        b3 = 0;

	        b3 = (tagBuffer[bPtr] & 0xff);
	        bPtr += 1;

	        return b3;
	    }

	    /**
	     * Reads eight unsigned bytes from file.
	     *
	     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
	     *                        endian.
	     *
	     * @return     The value of the double read from the file.
	     *
	     * @exception  IOException  if there is an error reading the file
	     */
	    public final double getDouble(boolean endianess) throws IOException {
	        b1 = (tagBuffer[bPtr] & 0xff);
	        b2 = (tagBuffer[bPtr + 1] & 0xff);
	        b3 = (tagBuffer[bPtr + 2] & 0xff);
	        b4 = (tagBuffer[bPtr + 3] & 0xff);
	        b5 = (tagBuffer[bPtr + 4] & 0xff);
	        b6 = (tagBuffer[bPtr + 5] & 0xff);
	        b7 = (tagBuffer[bPtr + 6] & 0xff);
	        b8 = (tagBuffer[bPtr + 7] & 0xff);

	        long tmpLong;

	        if (endianess == BIG_ENDIAN) {
	            tmpLong = (((long) b1 << 56) | ((long) b2 << 48) | ((long) b3 << 40) | ((long) b4 << 32) |
	                           ((long) b5 << 24) | ((long) b6 << 16) | ((long) b7 << 8) | b8);
	        } else {
	            tmpLong = (((long) b8 << 56) | ((long) b7 << 48) | ((long) b6 << 40) | ((long) b5 << 32) |
	                           ((long) b4 << 24) | ((long) b3 << 16) | ((long) b2 << 8) | b1);
	        }

	        bPtr += 8;

	        return (Double.longBitsToDouble(tmpLong));

	    }


	    /**
	     * Reads four unsigned bytes from file.
	     *
	     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
	     *                        endian.
	     *
	     * @return     The value of the float read from the file.
	     *
	     * @exception  IOException  if there is an error reading the file
	     */
	    public final float getFloat(boolean endianess) throws IOException {
	        int tmpInt;

	        if (endianess == BIG_ENDIAN) {
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
	     * Reads four signed bytes from file.
	     *
	     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
	     *                        endian.
	     *
	     * @return     The value of the integer read from the file.
	     *
	     * @exception  IOException  if there is an error reading the file
	     */
	    public final int getInt(boolean endianess) throws IOException {

	        b3 = 0;

	        if (endianess == BIG_ENDIAN) {
	            b3 = ((tagBuffer[bPtr] & 0xff) << 24) | ((tagBuffer[bPtr + 1] & 0xff) << 16) |
	                     ((tagBuffer[bPtr + 2] & 0xff) << 8) | (tagBuffer[bPtr + 3] & 0xff); // Big Endian
	        } else {
	            b3 = ((tagBuffer[bPtr + 3] & 0xff) << 24) | ((tagBuffer[bPtr + 2] & 0xff) << 16) |
	                     ((tagBuffer[bPtr + 1] & 0xff) << 8) | (tagBuffer[bPtr] & 0xff);
	        }

	        bPtr += 4;

	        return b3;
	    }

	    /**
	     * Reads eight unsigned bytes from file.
	     *
	     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
	     *                        endian.
	     *
	     * @return     The value of the long read from the file.
	     *
	     * @exception  IOException  if there is an error reading the file
	     */
	    public final long getLong(boolean endianess) throws IOException {
	        b1 = (tagBuffer[bPtr] & 0xff);
	        b2 = (tagBuffer[bPtr + 1] & 0xff);
	        b3 = (tagBuffer[bPtr + 2] & 0xff);
	        b4 = (tagBuffer[bPtr + 3] & 0xff);
	        b5 = (tagBuffer[bPtr + 4] & 0xff);
	        b6 = (tagBuffer[bPtr + 5] & 0xff);
	        b7 = (tagBuffer[bPtr + 6] & 0xff);
	        b8 = (tagBuffer[bPtr + 7] & 0xff);

	        long tmpLong;

	        if (endianess == BIG_ENDIAN) {
	            tmpLong = (((long) b1 << 56) | ((long) b2 << 48) | ((long) b3 << 40) | ((long) b4 << 32) |
	                           ((long) b5 << 24) | ((long) b6 << 16) | ((long) b7 << 8) | b8);
	        } else {
	            tmpLong = (((long) b8 << 56) | ((long) b7 << 48) | ((long) b6 << 40) | ((long) b5 << 32) |
	                           ((long) b4 << 24) | ((long) b3 << 16) | ((long) b2 << 8) | b1);
	        }

	        bPtr += 8;

	        return (tmpLong);
	    }

	    /**
	     * Reads two byte signed short from file.
	     *
	     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
	     *                        endian.
	     *
	     * @return     The value of signed short read from the file returned as an int.
	     *
	     * @exception  IOException  if there is an error reading the file
	     */
	    public final int getSignedShort(boolean endianess) throws IOException {
	        b3 = 0;

	        if (endianess == BIG_ENDIAN) {
	            b3 = ((tagBuffer[bPtr] & 0xff) << 8) | (tagBuffer[bPtr + 1] & 0xff);
	        } else {
	            b3 = ((tagBuffer[bPtr + 1] & 0xff) << 8) | (tagBuffer[bPtr] & 0xff);
	        }

	        if ((b3 & 0x0080) != 0) {
	            b3 = b3 | 0xff00;
	        }

	        bPtr += 2;

	        return b3;
	    }


	    /**
	     * Reads a string from a file of given <code>length</code>.
	     *
	     * @param      length  Number of bytes that form the string.
	     *
	     * @return     The string read from the file.
	     *
	     * @exception  IOException  if there is an error reading the file
	     */
	    public final String getString(int length) throws IOException {

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
	     * Reads a string from a file of given <code>length</code>.
	     *
	     * @param      length  Number of bytes that form the string.
	     *
	     * @return     The string read from the file.
	     *
	     * @exception  IOException  if there is an error reading the file
	     */
	    public final String getStringFromFile(int length) throws IOException {

	        if (length <= 0) {
	            return new String("");
	        }

	        byte[] b = new byte[length];
	        raFile.readFully(b);
	        String s = new String(b);
	        b = null;
	        return s;
	    }

	    /**
	     * Reads four unsigned bytes from file.
	     *
	     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
	     *                        endian.
	     *
	     * @return     The value of the integer read from the file.
	     *
	     * @exception  IOException  if there is an error reading the file
	     */
	    public final long getUInt(boolean endianess) throws IOException {

	        long val = 0;

	        if (endianess == BIG_ENDIAN) {
	            val = ((tagBuffer[bPtr] & 0xffL) << 24) | ((tagBuffer[bPtr + 1] & 0xffL) << 16) |
	                      ((tagBuffer[bPtr + 2] & 0xffL) << 8) | (tagBuffer[bPtr + 3] & 0xffL); // Big Endian
	        } else {
	            val = ((tagBuffer[bPtr + 3] & 0xffL) << 24) | ((tagBuffer[bPtr + 2] & 0xffL) << 16) |
	                      ((tagBuffer[bPtr + 1] & 0xffL) << 8) | (tagBuffer[bPtr] & 0xffL);
	        }

	        bPtr += 4;

	        return val;
	    }


	    /**
	     * Reads two unsigned bytes from file.
	     *
	     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
	     *                        endian.
	     *
	     * @return     The value of unsigned short read from the file returned as an int.
	     *
	     * @exception  IOException  if there is an error reading the file
	     */
	    public final int getUnsignedShort(boolean endianess) throws IOException {
	        b3 = 0;

	        if (endianess == BIG_ENDIAN) {
	            b3 = ((tagBuffer[bPtr] & 0xff) << 8) | (tagBuffer[bPtr + 1] & 0xff); // Big Endian
	        } else {
	            b3 = ((tagBuffer[bPtr + 1] & 0xff) << 8) | (tagBuffer[bPtr] & 0xff); // Little Endian
	        }

	        bPtr += 2;

	        return b3;
	    }

	    /**
	     * Setups the allocation of memory for the byte buffer to load the entire image.
	     */
	    public void initializeFullRead() {

	        try {
	            bPtr = 0;

	            if (raFile != null) {
	                fLength = raFile.length();
	            } else {
	                return;
	            }

	            if (tagBuffer == null) {
	                tagBuffer = new byte[(int) fLength];
	            } else if (fLength > tagBuffer.length) {
	                tagBuffer = new byte[(int) fLength];
	            }

	            raFile.readFully(tagBuffer);
	        } catch (IOException ioE) { }
	    }

	    /**
	     * Returns flag that indicates that the progressBar is visible.
	     *
	     * @return  <code>true</code> if progress bar is visible, <code>false</code> if not visible.
	     */
	    public boolean isProgressBarVisible() {
	        return pBarVisible;
	    }

	    /**
	     * Setups the allocation of memory for the byte buffer to load tags. This tag buffer is a cache to speed the loading
	     * of the images.
	     */
	    public void loadTagBuffer() {

	        try {
	            bPtr = 0;

	            if (raFile != null) {
	                fLength = raFile.length();
	            }

	            if (fLength < BUFFER_SIZE) {

	                if (tagBuffer == null) {
	                    tagBuffer = new byte[(int) fLength];
	                } else if (fLength > tagBuffer.length) {
	                    tagBuffer = new byte[(int) fLength];
	                }
	            } else if (tagBuffer == null) {
	                tagBuffer = new byte[BUFFER_SIZE];
	            } else if (tagBuffer.length < BUFFER_SIZE) {
	                tagBuffer = new byte[BUFFER_SIZE];
	            }

	            raFile.readFully(tagBuffer);
	        } catch (IOException ioE) { }
	    }

	    /**
	     * Sets byte buffer with int.
	     *
	     * @param  buffer     Byte buffers where data is to be stored.
	     * @param  data       Float data is broken down in bytes and stored in the byte buffer.
	     * @param  i          Index into byte buffer.
	     * @param  endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
	     */
	    public final void setBufferFloat(byte[] buffer, float data, int i, boolean endianess) {
	        int tmpInt;

	        tmpInt = Float.floatToIntBits(data);
	        setBufferInt(buffer, tmpInt, i, endianess);
	    }

	    /**
	     * Sets byte buffer with int.
	     *
	     * @param  buffer     Byte buffers where data is to be stored.
	     * @param  data       Integer data is broken down in bytes and stored in the byte buffer.
	     * @param  i          Index into byte buffer.
	     * @param  endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
	     */
	    public final void setBufferInt(byte[] buffer, int data, int i, boolean endianess) {

	        if (endianess == BIG_ENDIAN) {
	            buffer[i] = (byte) (data >>> 24);
	            buffer[i + 1] = (byte) (data >>> 16);
	            buffer[i + 2] = (byte) (data >>> 8);
	            buffer[i + 3] = (byte) (data & 0xff);
	        } else {
	            buffer[i] = (byte) (data & 0xff);
	            buffer[i + 1] = (byte) (data >>> 8);
	            buffer[i + 2] = (byte) (data >>> 16);
	            buffer[i + 3] = (byte) (data >>> 24);
	        }
	    }

	    /**
	     * Sets byte buffer with long.
	     *
	     * @param  buffer     Byte buffers where data is to be stored.
	     * @param  data       Long data is broken down in bytes and stored in the byte buffer.
	     * @param  i          Index into byte buffer.
	     * @param  endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
	     */
	    public final void setBufferLong(byte[] buffer, long data, int i, boolean endianess) {

	        if (endianess == BIG_ENDIAN) {
	            buffer[i] = (byte) (data >>> 56);
	            buffer[i + 1] = (byte) (data >>> 48);
	            buffer[i + 2] = (byte) (data >>> 40);
	            buffer[i + 3] = (byte) (data >>> 32);
	            buffer[i + 4] = (byte) (data >>> 24);
	            buffer[i + 5] = (byte) (data >>> 16);
	            buffer[i + 6] = (byte) (data >>> 8);
	            buffer[i + 7] = (byte) (data & 0xff);
	        } else {
	            buffer[i] = (byte) (data & 0xff);
	            buffer[i + 1] = (byte) (data >>> 8);
	            buffer[i + 2] = (byte) (data >>> 16);
	            buffer[i + 3] = (byte) (data >>> 24);
	            buffer[i + 4] = (byte) (data >>> 32);
	            buffer[i + 5] = (byte) (data >>> 40);
	            buffer[i + 6] = (byte) (data >>> 48);
	            buffer[i + 7] = (byte) (data >>> 56);
	        }
	    }


	    /**
	     * Sets byte buffer with int.
	     *
	     * @param  buffer     Byte buffers where data is to be stored.
	     * @param  data       Short data is broken down in bytes and stored in the byte buffer.
	     * @param  i          Index into byte buffer.
	     * @param  endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
	     */
	    public final void setBufferShort(byte[] buffer, short data, int i, boolean endianess) {

	        if (endianess == BIG_ENDIAN) {
	            buffer[i] = (byte) (data >>> 8);
	            buffer[i + 1] = (byte) (data & 0xff);
	        } else {
	            buffer[i] = (byte) (data & 0xff);
	            buffer[i + 1] = (byte) (data >>> 8);
	        }
	    }

	    /**
	     * Sets byte buffer with int.
	     *
	     * @param  buffer  Byte buffers where data is to be stored.
	     * @param  str     String containing integer data which is broken down in bytes and stored in the byte buffer.
	     * @param  i       Index into byte buffer.
	     */
	    public final void setBufferString(byte[] buffer, String str, int i) {

	        byte[] tmpBuffer;

	        tmpBuffer = str.getBytes();

	        for (int c = 0; c < tmpBuffer.length; c++) {
	            buffer[i + c] = tmpBuffer[c];
	        }
	    }

	    /**
	     * Sets whether or not the progress bar should be visible.
	     *
	     * @param  flag  <code>true</code> if should be visible, <code>false</code> if not visible.
	     */
	    public void setProgressBarVisible(boolean flag) {
	        pBarVisible = flag;
	    }

	    /**
	     * DOCUMENT ME!
	     *
	     * @param  buffer  byte[]
	     */
	    public final void setTagBuffer(byte[] buffer) {
	        tagBuffer = buffer;
	        fLength = buffer.length;
	    }

	    /**
	     * Writes a double as eight bytes to a file.
	     *
	     * @param      data       Data to be written to file.
	     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
	     *                        endian.
	     *
	     * @exception  IOException  if there is an error writing the file
	     */
	    public final void writeDouble(double data, boolean endianess) throws IOException {

	        long tmpLong;

	        tmpLong = Double.doubleToLongBits(data);
	        writeLong(tmpLong, endianess);
	    }

	    /**
	     * Writes a float as four bytes to a file.
	     *
	     * @param      data       Data to be written to file.
	     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
	     *                        endian.
	     *
	     * @exception  IOException  if there is an error writing the file
	     */
	    public final void writeFloat(float data, boolean endianess) throws IOException {
	        int tmpInt;

	        tmpInt = Float.floatToIntBits(data);
	        writeInt(tmpInt, endianess);
	    }

	    /**
	     * Writes an int as four bytes to a file.
	     *
	     * @param      data       Data to be written to file.
	     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
	     *                        endian.
	     *
	     * @exception  IOException  if there is an error writing the file
	     */
	    public final void writeInt(int data, boolean endianess) throws IOException {

	        if (endianess == BIG_ENDIAN) {
	            byteBuffer4[0] = (byte) (data >>> 24);
	            byteBuffer4[1] = (byte) (data >>> 16);
	            byteBuffer4[2] = (byte) (data >>> 8);
	            byteBuffer4[3] = (byte) (data & 0xff);
	        } else {
	            byteBuffer4[0] = (byte) (data & 0xff);
	            byteBuffer4[1] = (byte) (data >>> 8);
	            byteBuffer4[2] = (byte) (data >>> 16);
	            byteBuffer4[3] = (byte) (data >>> 24);
	        }

	        raFile.write(byteBuffer4);
	    }

	    /**
	     * Writes a long as eight bytes to a file.
	     *
	     * @param      data       Data to be written to file.
	     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
	     *                        endian.
	     *
	     * @exception  IOException  if there is an error writing the file
	     */
	    public final void writeLong(long data, boolean endianess) throws IOException {

	        if (endianess == BIG_ENDIAN) {
	            byteBuffer8[0] = (byte) (data >>> 56);
	            byteBuffer8[1] = (byte) (data >>> 48);
	            byteBuffer8[2] = (byte) (data >>> 40);
	            byteBuffer8[3] = (byte) (data >>> 32);
	            byteBuffer8[4] = (byte) (data >>> 24);
	            byteBuffer8[5] = (byte) (data >>> 16);
	            byteBuffer8[6] = (byte) (data >>> 8);
	            byteBuffer8[7] = (byte) (data & 0xff);
	        } else {
	            byteBuffer8[0] = (byte) (data & 0xff);
	            byteBuffer8[1] = (byte) (data >>> 8);
	            byteBuffer8[2] = (byte) (data >>> 16);
	            byteBuffer8[3] = (byte) (data >>> 24);
	            byteBuffer8[4] = (byte) (data >>> 32);
	            byteBuffer8[5] = (byte) (data >>> 40);
	            byteBuffer8[6] = (byte) (data >>> 48);
	            byteBuffer8[7] = (byte) (data >>> 56);
	        }

	        raFile.write(byteBuffer8);
	    }


	    /**
	     * Writes a short as two bytes to a file.
	     *
	     * @param      data       Data to be written to file.
	     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
	     *                        endian.
	     *
	     * @exception  IOException  if there is an error writing the file
	     */
	    public final void writeShort(short data, boolean endianess) throws IOException {

	        if (endianess == BIG_ENDIAN) {
	            byteBuffer2[0] = (byte) (data >>> 8);
	            byteBuffer2[1] = (byte) (data & 0xff);
	        } else {
	            byteBuffer2[0] = (byte) (data & 0xff);
	            byteBuffer2[1] = (byte) (data >>> 8);
	        }

	        raFile.write(byteBuffer2);
	    }

	    /**
	     * Gets the file/buffer pointer and returns it.
	     *
	     * @return  the file/buffer pointer
	     */
	    protected final int getFilePointer() {
	        return bPtr;
	    }


	    /**
	     * Reads into the supplied buffer data from the DICOM tag buffer.
	     *
	     * @param  byteBuffer  byte[]
	     */
	    protected final void read(byte[] byteBuffer) {

	        System.arraycopy(tagBuffer, bPtr, byteBuffer, 0, byteBuffer.length);
	        bPtr += byteBuffer.length;
	    }

	    /**
	     * Seeks to a point in the buffer.
	     *
	     * @param  value  indicates the new buffer pointer value
	     */
	    protected final void seek(int value) {
	        bPtr = value;
	    }

	    /**
	     * Skips to a new point in the buffer.
	     *
	     * @param  value  number of bytes to skip
	     */
	    protected final void skipBytes(int value) {
	        bPtr = bPtr + value;
	    }

	}

	
	
	
	
	/**
	 * Validator to test accuracy
	 * @param args
	 * @throws IOException
	 */
	public static void main(String[] args) throws IOException {
		
		byte[] ar = {0, 0, 5, 0, 70, 68, 8, 0, 0, 0, 0, 0, 0, 26, 117, 64, 0, 0, 6, 0, 70, 68, 8,0, 41, 92, -113, -62, -11, -88, 18, 64, 0, 0, 7, 0, 83, 76, 4, 0, -69, 3, 0, 0, 0, 0, 8, 0, 83, 76, 4 ,0, -56, 0, 0 ,0, 0, 0, 13, 0 ,83, 76, 4, 0, 2, 0, 0, 0, 0 ,0, 14, 0, 83, 76, 4, 0, 4 ,0, 0, 0, 0, 0 ,19, 0 ,83, 76, 4, 0 ,81, 1, 0, 0, 0 ,0, 20, 0, 83, 76, 4 ,0, 1, 0, 0, 0 ,0, 0, 22, 0, 83, 76, 4 ,0, 50, 0 ,0, 0, 0, 0, 24, 0, 83, 76, 4, 0, -96, 2, 0, 0 ,0, 0, 29, 0, 76, 84, 14, 0, 65, 0, 66, 0, 68, 0, 79, 0, 77, 0, 69, 0 ,78, 0, 0, 0, 30, 0, 70, 68, 8, 0, 0, 0, 0, 0, 0, 64, 127, 64, 0, 0, 31, 0 ,70, 68, 8, 0, 0, 0, 0, 0, 0 ,0 ,0 ,0 ,0 ,0, 32, 0, 70, 68, 8 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0, 0, 33, 0, 83, 76, 4 ,0 ,1 ,0, 0, 0, 0, 0, 34, 0, 83, 76, 4, 0, 3, 0, 0, 0, 0, 0, 37 ,0 ,83, 76, 4, 0, 1 ,0, 0, 0, 0, 0, 38, 0, 70, 68, 8 ,0 ,0, 0, 0, 0, 0, 112 ,124, 64 ,0, 0, 39, 0, 70, 68, 8 ,0 ,0 ,0 ,0, 0, 0, 0, 78, 64, 0, 0, 40, 0, 83, 76, 4 ,0, 38, 1, 0, 0, 0, 0, 41, 0, 83, 76, 4 ,0, 0, 0, 0, 0, 0, 0, 44, 0, 83, 76, 4, 0, -44, 0, 0, 0 ,0 ,0, 45, 0, 83, 76, 4, 0, 0, 0, 0, 0, 0, 0, 46, 0, 83, 76, 4, 0, 1, 0, 0, 0, 0, 0, 47, 0, 70, 68, 8, 0, 0, 0, 0, 0, 0, 0, 4 ,64, 0, 0, 48, 0, 76, 84, 8, 0, 80, 0, 50, 0, 48, 0, 65, 0, 0, 0 ,51, 0, 83, 76, 4, 0, 3, 0 ,0 ,0 ,0 ,0 ,1, 1, 70, 68, 8, 0, 0 ,0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 1, 70, 68, 8, 0, 14, 45, -78, -99, -17, -89, -18, 63, 0, 0, 3, 1, 70 ,68 ,8, 0, -123, -21, 81, -72, 30, -123, -21, 63, 0, 0, 5, 1, 73, 83, 2, 0, 48, 32, -1, -1, -1, -1, 67, 83, 10, 0, 69, 78, 68, 33, 32, 32, 32, 32, 32, 32};
		
		String str = new String(ar);
		System.out.println("The string is:");
		System.out.println(str);
		
		/*File[] f = new File[1];
		
		f[0] = new File(args[0]);
		String[] s = new String[1];
		s[0] = "";
		
		PlugInAlgorithmAnonymizeDicom p = new PlugInAlgorithmAnonymizeDicom (f, s, "", "");
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
		}*/
		
	}
	

}
