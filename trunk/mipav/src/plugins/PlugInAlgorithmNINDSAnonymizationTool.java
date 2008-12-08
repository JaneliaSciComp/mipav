import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.DicomDictionary;
import gov.nih.mipav.model.file.FileDicomKey;
import gov.nih.mipav.model.file.FileDicomSQ;
import gov.nih.mipav.model.file.FileDicomTag;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

import javax.swing.JLabel;
import javax.swing.JTextArea;

/**
 * @author pandyan
 *
 */
public class PlugInAlgorithmNINDSAnonymizationTool extends AlgorithmBase {
	
	/** input directory path **/
	private String inputDirectoryPath;
	
	/** output top level directory path **/
	private String outputDirectoryPath;
	
	/** output text area **/
	private JTextArea outputTextArea;
    
    /** boolean indicating if methods were successfule **/
    private boolean success = false;

    /** input image **/
    private ModelImage inputImage;
    
    /** handle to FileIO **/
    private FileIO fileIO;
    
    /** handle to FileInfoDicom **/
    private FileInfoDicom fileInfoDicom;
    
    /** output File **/
    private File outputFile;
    
    /** output stream **/
    private FileOutputStream outputStream;
    
    /** print stream **/
    private PrintStream printStream;
    
    /** outputText filename **/
    private String outputTextFileName;
    
    /** message label **/
    private JLabel errorMessageLabel;
    
    /** boolean if alg was canceled **/
    private boolean algCanceled = false;
    
    /** enable text area boolean **/
    private boolean enableTextArea;
    
    /** tag table **/
    private FileDicomTagTable tagTable;
    
    /** boolean indicating whether to change greadGrandParentFileDir name to the newUID **/
    private boolean renameGrandParentDir;
    
    /** newUID String **/
    private String newUID;
    
    private static Vector<FileDicomKey> removeTagsVector;
    private static Vector<FileDicomKey> replaceTagsVector;
    private static Vector<FileDicomKey> encryptTagsVector;
    private static FileDicomKey key;
    
    private static FileDicomKey patientIDKey;
    private static FileDicomKey patientDOBKey;
    private static FileDicomKey patientAgeKey;  
    private static FileDicomKey studyDateKey;
    
    
    static {
    	removeTagsVector = new Vector<FileDicomKey>();
    	key = new FileDicomKey("0008,0021");
    	removeTagsVector.add(key);
    	key = new FileDicomKey("0008,0022");
    	removeTagsVector.add(key);
    	key = new FileDicomKey("0008,0023");
    	removeTagsVector.add(key);
    	key = new FileDicomKey("0008,0081");
    	removeTagsVector.add(key);
    	key = new FileDicomKey("0008,0092");
    	removeTagsVector.add(key);
    	key = new FileDicomKey("0008,0094");
    	removeTagsVector.add(key);
    	key = new FileDicomKey("0008,1155");
    	removeTagsVector.add(key);
    	key = new FileDicomKey("0010,0032");
    	removeTagsVector.add(key);
    	key = new FileDicomKey("0010,1000");
    	removeTagsVector.add(key);
    	key = new FileDicomKey("0010,1001");
    	removeTagsVector.add(key);
    	key = new FileDicomKey("0010,2180");
    	removeTagsVector.add(key);
    	key = new FileDicomKey("0010,21B0");
    	removeTagsVector.add(key);
    	key = new FileDicomKey("0010,4000");
    	removeTagsVector.add(key);
    	key = new FileDicomKey("0040,0275");
    	removeTagsVector.add(key);
    	key = new FileDicomKey("0040,A730");
    	removeTagsVector.add(key);
    	
    	
    	replaceTagsVector = new Vector<FileDicomKey>();
    	key = new FileDicomKey("0002,0003");
    	replaceTagsVector.add(key);
    	key = new FileDicomKey("0002,0012");
    	replaceTagsVector.add(key);
    	key = new FileDicomKey("0008,0012");
    	replaceTagsVector.add(key);
    	key = new FileDicomKey("0008,0013");
    	replaceTagsVector.add(key);
    	key = new FileDicomKey("0008,0014");
    	replaceTagsVector.add(key);
    	key = new FileDicomKey("0008,0018");
    	replaceTagsVector.add(key);
    	key = new FileDicomKey("0008,0020"); 
    	replaceTagsVector.add(key);
    	key = new FileDicomKey("0010,0010");
    	replaceTagsVector.add(key);
    	key = new FileDicomKey("0010,0020");
    	replaceTagsVector.add(key);
    	key = new FileDicomKey("0010,0030");
    	replaceTagsVector.add(key);
    	key = new FileDicomKey("0020,000D");
    	replaceTagsVector.add(key);
    	key = new FileDicomKey("0020,000E");
    	replaceTagsVector.add(key);
    	key = new FileDicomKey("0020,0052");
    	replaceTagsVector.add(key);
    	key = new FileDicomKey("0020,0200");
    	replaceTagsVector.add(key);
    	key = new FileDicomKey("0040,A124");
    	replaceTagsVector.add(key);
    	key = new FileDicomKey("0088,0140");
    	replaceTagsVector.add(key);
    	key = new FileDicomKey("3006,0024");
    	replaceTagsVector.add(key);
    	key = new FileDicomKey("3006,00C2");
    	replaceTagsVector.add(key);
    	
    	
    	encryptTagsVector = new Vector<FileDicomKey>();
    	key = new FileDicomKey("0008,0050");
    	encryptTagsVector.add(key);
    	key = new FileDicomKey("0008,0080");
    	encryptTagsVector.add(key);
    	key = new FileDicomKey("0008,0090");
    	encryptTagsVector.add(key);
    	key = new FileDicomKey("0008,1010");
    	encryptTagsVector.add(key);
    	key = new FileDicomKey("0008,1040");
    	encryptTagsVector.add(key);
    	key = new FileDicomKey("0008,1048");
    	encryptTagsVector.add(key);
    	key = new FileDicomKey("0008,1050");
    	encryptTagsVector.add(key);
    	key = new FileDicomKey("0008,1060");
    	encryptTagsVector.add(key);
    	key = new FileDicomKey("0008,1070");
    	encryptTagsVector.add(key);
    	key = new FileDicomKey("0008,1090");
    	encryptTagsVector.add(key);
    	key = new FileDicomKey("0018,1000");
    	encryptTagsVector.add(key);
    	
    	patientIDKey = new FileDicomKey("0010,0020");
    	patientDOBKey = new FileDicomKey("0010,0030");
    	patientAgeKey = new FileDicomKey("0010,1010");
    	studyDateKey = new FileDicomKey("0008,0020");
    	
    	
    	
    	
    	
    }
    
    
	
	/**
	 * constructor
	 */
	public PlugInAlgorithmNINDSAnonymizationTool(String inputDirectoryPath, String outputDirectoryPath, JTextArea outputTextArea, JLabel errorMessageLabel, boolean enableTextArea, boolean renameGrandParentDir) {
		this.inputDirectoryPath = inputDirectoryPath;
		this.outputDirectoryPath = outputDirectoryPath;
		this.outputTextArea = outputTextArea;
		this.errorMessageLabel = errorMessageLabel;
		this.enableTextArea = enableTextArea;
		this.renameGrandParentDir = renameGrandParentDir;

		fileIO = new FileIO();
		fileIO.setQuiet(true);
		
		//remove last slash from input directory path if it has it
        if(String.valueOf(inputDirectoryPath.charAt(inputDirectoryPath.length() - 1)).equals(File.separator)) {
        	inputDirectoryPath = inputDirectoryPath.substring(0,inputDirectoryPath.length() - 1);
	    }
        //remove last slash from output directory  path if it has it
        if(String.valueOf(outputDirectoryPath.charAt(outputDirectoryPath.length() - 1)).equals(File.separator)) {
        	outputDirectoryPath = outputDirectoryPath.substring(0,outputDirectoryPath.length() - 1);
	    }
 
	}
	
	

	/**
	 * run algorithm
	 */
	public void runAlgorithm() {
		long begTime = System.currentTimeMillis();

		try {
			outputTextFileName = "output_" + System.currentTimeMillis() + ".txt";
			outputFile = new File(inputDirectoryPath + File.separator + outputTextFileName);
        	outputStream = new FileOutputStream(outputFile);
        	printStream = new PrintStream(outputStream);
        }catch(Exception e) {
        	
        }
		
		DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");
        Date date = new Date();
        if(enableTextArea) {
        	outputTextArea.append(dateFormat.format(date) + "\n\n");
        }
        printStream.println(dateFormat.format(date));
        printStream.println();
        
        if(enableTextArea) {
        	outputTextArea.append("** Beginning NINDS Anonymization **\n\n");
        }
        printStream.println("** Beginning NINDS Anonymization **");
        printStream.println();
        
        if(enableTextArea) {
        	outputTextArea.append("Input Directory is " + inputDirectoryPath + "\n");
        }
        printStream.println("Input Directory is " + inputDirectoryPath);
        
        if(enableTextArea) {
        	outputTextArea.append("Output Directory is " + outputDirectoryPath + "\n\n");
        }
        printStream.println("Output Directory is " + outputDirectoryPath);
        printStream.println();

        
        
        // first create a File object based upon the study path
        File inputDirectoryRoot = new File(inputDirectoryPath);
        

        success = parse(inputDirectoryRoot);

        if (success == false) {
        	if(enableTextArea) {
        		outputTextArea.append("! Algorithm Canceled \n");
        	}
        	errorMessageLabel.setText("! Algorithm Canceled");
        	printStream.println("! Algorithm Canceled");
        	finalize();
            setCompleted(true);
        }

        if(enableTextArea) {
        	outputTextArea.append("** Ending NINDS Anonymization **\n\n");
        }
        printStream.println("** Ending NINDS Anonymization **");
        printStream.println();



        long endTime = System.currentTimeMillis();
        long diffTime = endTime - begTime;
        float seconds = ((float) diffTime) / 1000;

       if(enableTextArea) {
    	   outputTextArea.append("Algorithm took " + seconds + " seconds \n");
       }
       printStream.println("Algorithm took " + seconds + " seconds");
       
       finalize();
       setCompleted(true);
	}
	
	
	
	/**
	 * parses the input directory and anonymizes image files
	 * @param file
	 * @return
	 * @throws IOException
	 * @throws OutOfMemoryError
	 */
	 public boolean parse(File file) {

	        File[] children = file.listFiles();

	        for (int i = 0; i < children.length; i++) {

                if (algCanceled) {
                    return false;
                }

                if (children[i].isDirectory()) {
                	//create this directory in output directory if its not there yet
                	File test = new File(children[i].getAbsolutePath().replace(inputDirectoryPath, outputDirectoryPath));
                	if(!test.exists()) {
                		test.mkdir();
                	}
                    parse(children[i]);
                } else if (!children[i].isDirectory()) {
                		try {
	                		if((FileUtility.isDicom(children[i].getName(), children[i].getParent() + File.separator, true) == FileUtility.DICOM) || (FileUtility.isDicom_ver2(children[i].getName(), children[i].getParent() + File.separator, true) == FileUtility.DICOM)) {

	                			success = anonymizeDICOM(children[i]);

	                	        if (success == false) {
	                				if(enableTextArea) {
	                					outputTextArea.append("!!!!!!!!!!!!!!!!!!!! ERROR IN ANONYMIZING " + children[i].getName() + " \n\n");
	                				}
	                				errorMessageLabel.setText("! Error in anonymizing 1 or more image files");
	
	                			
	                				printStream.println("!!!!!!!!!!!!!!!!!!!! ERROR IN ANONYMIZING " + children[i].getName());
	                				printStream.println();
	                	            continue;
	                	        }
	                			
	                		}
                		}catch(IOException e) {
                			if(enableTextArea) {
                				outputTextArea.append("!!!!!!!!!!!!!!!!!!!! IO Error in determing if file is DICOM : " + children[i].getName() + " \n\n");
                			}
                			
            				printStream.println("!!!!!!!!!!!!!!!!!!!! IO Error in determing if file is DICOM : " + children[i].getName());
            				printStream.println();
            	            continue;
                		}
                	
                }
                
	        }

	        return true;
	 }
	
	
	
	
	
	
	/**
	 * anonymize dicom images
	 * @return
	 */
	public boolean anonymizeDICOM(File file) {
		//read in image
		if(enableTextArea) {
			outputTextArea.append("Reading in " + file.getName() + " from " + file.getParent() + " \n");
		}
		printStream.println("Reading in " + file.getName() + " from " + file.getParent());
		String absPath = file.getAbsolutePath();
		inputImage = fileIO.readImage(absPath);
		fileInfoDicom = (FileInfoDicom)inputImage.getFileInfo(0);
		
		//anonymize
		if(enableTextArea) {
			outputTextArea.append("Anonymizing " + file.getName() + " \n");
		}
		printStream.println("Anonymizing " + file.getName());
		success = anonymizeDICOMTags();
		if (success == false) {
			fileInfoDicom.finalize();
			inputImage.disposeLocal();
	        inputImage = null;
            return false;
        }
		
		//save anonymized image
		String outputDir = file.getParent().replace(inputDirectoryPath, outputDirectoryPath);
		
		//rename grandParentDir name with newUID if user chooses to
		if(renameGrandParentDir) {
			File outputFile = new File(outputDir);
			File parentFile = outputFile.getParentFile();
			File grandParentFile = parentFile.getParentFile();
			if(!(grandParentFile.getName().equals(newUID))) {
				String newName = grandParentFile.getParent() + File.separator + newUID;
				File dest = new File(newName);
				boolean succ = grandParentFile.renameTo(dest);
				//if succ is false, then a folder already exists...so delete this dir
				if(succ == false) {

					delete(grandParentFile);
				}
			}
			outputDir = outputDir.replace(grandParentFile.getName(), newUID);
		}
	
		FileWriteOptions opts = new FileWriteOptions(true);
        opts.setFileType(FileUtility.DICOM);
        opts.setFileDirectory(outputDir + File.separator);
        opts.setFileName(file.getName());
        opts.setRecalculateInstanceNumber(false);
        if(enableTextArea) {
        	outputTextArea.append("Saving " + file.getName() + " to " + outputDir + " \n\n");
        }
        printStream.println("Saving " + file.getName() + " to " + outputDir);
        printStream.println();
        fileIO.setDisplayRangeOfSlicesDialog(false);
        fileIO.writeImage(inputImage, opts);
        

        inputImage.disposeLocal();
        inputImage = null;
        fileInfoDicom = null;
        
		return true;
	}
	
	
	/**
	 * anonymize dicom images
	 * this method does the actual anonymizing part
	 * @return
	 */
    public boolean anonymizeDICOMTags() {
    	tagTable = fileInfoDicom.getTagTable();
    	int size;

    	//couple of the tags (patient name (0010,0010)and patient id(0010,0020) will be replaced with a new UID which is Suject ID + DOB
    	//example: id = 1234, DOB=9/23/1968 => 1234 + 9231968 = 9233202
    	String patientID = "";
    	String dob = "";
    	newUID = "";
    	boolean containsDOB = false;
    	int patientIDInt = 0;
    	if(tagTable.containsTag(patientIDKey)) {
    		patientID = ((String)tagTable.getValue(patientIDKey)).trim();
    		
    	}
		try {
			patientIDInt = new Integer(patientID).intValue();
		}catch(NumberFormatException e) {
			if(enableTextArea) {
				outputTextArea.append("! Patient ID(0010,0020) value is not a valid entry \n");
			}
			printStream.println("! Patient ID(0010,0020) value is not a valid entry");
			e.printStackTrace();
			return false;
		}
		
		
		if(tagTable.containsTag(patientDOBKey)) {
			dob = ((String)tagTable.getValue(patientDOBKey)).trim();
		}
		
		//if dob is there, create newUID using this field....otherwise use studydate
		if(!dob.equals("")) {
			if(dob.contains("/")) {
				dob = dob.replaceAll("\\/", "");
			}
			int dobInt = 0;
			try {
				dobInt = new Integer(dob).intValue();
			}catch(NumberFormatException e) {
				if(enableTextArea) {
					outputTextArea.append("! Patient DOB(0010,0030) value is not a valid entry \n");
				}
				printStream.println("! Patient DOB(0010,0030) value is not a valid entry");
				e.printStackTrace();
				return false;
			}
			int newUIDInt = patientIDInt + dobInt;
			newUID = String.valueOf(newUIDInt);
		}else {

			String sDate = ((String)tagTable.getValue(studyDateKey)).trim();
			
			if(sDate.contains("/")) {
				sDate = sDate.replaceAll("\\/", "");
			}
			int sDateInt = 0;
			try {
				sDateInt = new Integer(sDate).intValue();
			}catch(NumberFormatException e) {
				if(enableTextArea) {
					outputTextArea.append("! Study Date(0008,0020) value is not a valid entry \n");
				}
				printStream.println("! Study Date(0008,0020) value is not a valid entry");
				e.printStackTrace();
				return false;
			}
			int newUIDInt = patientIDInt + sDateInt;
			newUID = String.valueOf(newUIDInt);
		}


    	//one will get replaced with current date...so get current date
    	DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy");
        Date date = new Date();
        String currentDate = dateFormat.format(date);

        //one tag will get replaced with current time...so get that
        DateFormat timeFormat = new SimpleDateFormat("HHmmss");
        date = new Date();
        String currentTime = timeFormat.format(date);
        
        //bogus SPOID
        String bogusSOPID = "1.2.840.999999999999999999";
        
        //bogus implementation ID
        String bogusImplementationID =  "1.2.840.34379.17";

        //MD5
        MessageDigest digest = null;
        try {
        	digest = MessageDigest.getInstance("MD5");
        }catch(NoSuchAlgorithmException e) {
        	if(enableTextArea) {
        		outputTextArea.append("! Error in MD5 hash algorithm \n");
        	}
			printStream.println("! Error in MD5 hash algorithm");
        	e.printStackTrace();
        	return false;
        }

        //patient's birthdate (0010,0030) will be replaced with patients age...so get patients age
        String patientsAge = "";
        if(tagTable.containsTag(patientAgeKey)) {
        	patientsAge = (String)tagTable.getValue(patientAgeKey);
        }
        
        //Study Instance UID (0020,000D) and Series Instance UID (0002,000E) need MIPAV version and time in milliseconds
        String studyInstanceUID = "";
        String seriesInstanceUID = "";
        String mipavVersion = MipavUtil.getVersion();
        mipavVersion = mipavVersion.replaceAll("\\.", "");
        long time = date.getTime();
    	studyInstanceUID = "1.2.840.9999.9." + mipavVersion + "." + time + ".0";
    	seriesInstanceUID = "1.2.840.9999.9." + mipavVersion + "." + time + ".1";
    	
    	
    	
    	
    	
    	
    	//replace specific defined tags
    	size = replaceTagsVector.size();
    	String s;
    	for (int i = 0; i < size; i++) {
        	FileDicomKey key = replaceTagsVector.get(i);
        	if(tagTable.containsTag(key)) {
        		String keyString = key.getKey();
        		if(keyString.equals("0002,0003")) {
        			s = (String)tagTable.getValue(key);
        			if(enableTextArea) {
            			outputTextArea.append("- Replacing (0002,0003) value of " + s + " to " + bogusSOPID  + " \n");
            		}
            		printStream.println("- Replacing (0002,0003) value of " + s + " to " + bogusSOPID);
            		tagTable.setValue(key, bogusSOPID);
        		}else if(keyString.equals("0002,0012")) {
        			s = (String)tagTable.getValue(key);
        			if(enableTextArea) {
            			outputTextArea.append("- Replacing (0002,0012) value of " + s + " to " + bogusImplementationID  + " \n");
            		}
            		printStream.println("- Replacing (0002,0012) value of " + s + " to " + bogusImplementationID);
            		tagTable.setValue(key, bogusImplementationID);
        		}else if(keyString.equals("0008,0012")) {
        			s = (String)tagTable.getValue(key);
        			if(enableTextArea) {
            			outputTextArea.append("- Replacing (0008,0012) value of " + s + " to " + currentDate  + " \n");
            		}
            		printStream.println("- Replacing (0008,0012) value of " + s + " to " + currentDate);
            		tagTable.setValue(key, currentDate);
        		}else if(keyString.equals("0008,0013")) {
        			s = (String)tagTable.getValue(key);
        			if(enableTextArea) {
            			outputTextArea.append("- Replacing (0008,0013) value of " + s + " to " + currentTime  + " \n");
            		}
            		printStream.println("- Replacing (0008,0013) value of " + s + " to " + currentTime);
            		tagTable.setValue(key, currentTime);
        			
        		}else if(keyString.equals("0008,0014")) {
        			s = (String)tagTable.getValue(key);
        			if(enableTextArea) {
            			outputTextArea.append("- Replacing (0008,0014) value of " + s + " to " + bogusImplementationID  + " \n");
            		}
            		printStream.println("- Replacing (0008,0014) value of " + s + " to " + bogusImplementationID);
            		tagTable.setValue(key, bogusImplementationID);
        		}else if(keyString.equals("0008,0018")) {
        			s = (String)tagTable.getValue(key);
        			if(enableTextArea) {
            			outputTextArea.append("- Replacing (0008,0018) value of " + s + " to " + bogusSOPID  + " \n");
            		}
            		printStream.println("- Replacing (0008,0018) value of " + s + " to " + bogusSOPID);
            		tagTable.setValue(key, bogusSOPID);
        		}else if(keyString.equals("0008,0020")) {
        			s = (String)tagTable.getValue(key);
        			String k = s.substring(0, s.lastIndexOf("/")+1) + "1000";
            		if(enableTextArea) {
            			outputTextArea.append("- Replacing (0008,0020) value of " + s + " to " + k  + " \n");
            		}
            		printStream.println("- Replacing (0008,0020) value of " + s + " to " + k );
            		tagTable.setValue(key, k);
        		}else if(keyString.equals("0010,0010")) {
        			s = (String)tagTable.getValue(key);
        			if(enableTextArea) {
            			outputTextArea.append("- Replacing (0010,0010) value of " + s + " to " + newUID  + " \n");
            		}
            		printStream.println("- Replacing (0010,0010) value of " + s + " to " + newUID);
            		tagTable.setValue(key, newUID);
        		}else if(keyString.equals("0010,0020")) {
        			s = (String)tagTable.getValue(key);
        			if(enableTextArea) {
            			outputTextArea.append("- Replacing (0010,0020) value of " + s + " to " + newUID  + " \n");
            		}
            		printStream.println("- Replacing (0010,0020) value of " + s + " to " + newUID);
            		tagTable.setValue(key, newUID);
        		}else if(keyString.equals("0010,0030")) {
        			s = (String)tagTable.getValue(key);
        			if(enableTextArea) {
            			outputTextArea.append("- Replacing (0010,0030) value of " + s + " to " + patientsAge  + " \n");
            		}
            		printStream.println("- Replacing (0010,0030) value of " + s + " to " + patientsAge);
            		tagTable.setValue(key, patientsAge);
        		}else if(keyString.equals("0020,000D")) {
        			s = (String)tagTable.getValue(key);
        			if(enableTextArea) {
            			outputTextArea.append("- Replacing (0020,000D) value of " + s + " to " + studyInstanceUID  + " \n");
            		}
            		printStream.println("- Replacing (0020,000D) value of " + s + " to " + studyInstanceUID);
            		tagTable.setValue(key, studyInstanceUID);
        		}else if(keyString.equals("0020,000E")) {
        			s = (String)tagTable.getValue(key);
        			if(enableTextArea) {
            			outputTextArea.append("- Replacing (0020,000E) value of " + s + " to " + seriesInstanceUID  + " \n");
            		}
            		printStream.println("- Replacing (0020,000E) value of " + s + " to " + seriesInstanceUID);
            		tagTable.setValue(key, seriesInstanceUID);
        		}else if(keyString.equals("0020,0052")) {
        			s = ((String)tagTable.getValue(key)).trim();
        			int x = s.lastIndexOf(".");
            		if(x != -1) {
            			String k = s.substring(0, x);
            			s = k + ".99999";
            		}else {
            			//need to figure out what to do here
            			//for now...just seet it to 99999
            			s = "99999";
            		}
            		if(enableTextArea) {
            			outputTextArea.append("- Replacing (0020,0052) value of " + s + " to " + s  + " \n");
            		}
            		printStream.println("- Replacing (0020,0052) value of " + s + " to " + s);
            		tagTable.setValue(key, s);
        			
        		}else if(keyString.equals("0020,0200")) {
        			s = ((String)tagTable.getValue(key)).trim();
        			int x = s.lastIndexOf(".");
            		if(x != -1) {
            			String k = s.substring(0, x);
            			s = k + ".99999";
            		}else {
            			//need to figure out what to do here
            			//for now...just seet it to 99999
            			s = "99999";
            		}
            		if(enableTextArea) {
            			outputTextArea.append("- Replacing (0020,0200) value of " + s + " to " + s  + " \n");
            		}
            		printStream.println("- Replacing (0020,0200) value of " + s + " to " + s);
            		tagTable.setValue(key, s);
        			
        		}else if(keyString.equals("0040,A124")) {
        			s = ((String)tagTable.getValue(key)).trim();
        			int x = s.lastIndexOf(".");
            		if(x != -1) {
            			String k = s.substring(0, x);
            			s = k + ".99999";
            		}else {
            			//need to figure out what to do here
            			//for now...just seet it to 99999
            			s = "99999";
            		}
            		if(enableTextArea) {
            			outputTextArea.append("- Replacing (0040,A124) value of " + s + " to " + s  + " \n");
            		}
            		printStream.println("- Replacing (0040,A124) value of " + s + " to " + s);
            		tagTable.setValue(key, s);
        			
        		}else if(keyString.equals("0088,0140")) {
        			s = ((String)tagTable.getValue(key)).trim();
        			int x = s.lastIndexOf(".");
            		if(x != -1) {
            			String k = s.substring(0, x);
            			s = k + ".99999";
            		}else {
            			//need to figure out what to do here
            			//for now...just seet it to 99999
            			s = "99999";
            		}
            		if(enableTextArea) {
            			outputTextArea.append("- Replacing (0088,0140) value of " + s + " to " + s  + " \n");
            		}
            		printStream.println("- Replacing (0088,0140) value of " + s + " to " + s);
            		tagTable.setValue(key, s);
        		}else if(keyString.equals("3006,0024")) {
        			s = ((String)tagTable.getValue(key)).trim();
        			int x = s.lastIndexOf(".");
            		if(x != -1) {
            			String k = s.substring(0, x);
            			s = k + ".99999";
            		}else {
            			//need to figure out what to do here
            			//for now...just seet it to 99999
            			s = "99999";
            		}
            		if(enableTextArea) {
            			outputTextArea.append("- Replacing (3006,0024) value of " + s + " to " + s  + " \n");
            		}
            		printStream.println("- Replacing (3006,0024) value of " + s + " to " + s);
            		tagTable.setValue(key, s);
        		}else if(keyString.equals("3006,00C2")) {
        			s = ((String)tagTable.getValue(key)).trim();
        			int x = s.lastIndexOf(".");
            		if(x != -1) {
            			String k = s.substring(0, x);
            			s = k + ".99999";
            		}else {
            			//need to figure out what to do here
            			//for now...just seet it to 99999
            			s = "99999";
            		}
            		if(enableTextArea) {
            			outputTextArea.append("- Replacing (3006,00C2) value of " + s + " to " + s  + " \n");
            		}
            		printStream.println("- Replacing (3006,00C2) value of " + s + " to " + s);
            		tagTable.setValue(key, s);
        		}
        	}
    	}
    	
    	
    	
    	
    	//encrypt specific defined tags
    	size = encryptTagsVector.size();
    	for (int i = 0; i < size; i++) {
        	FileDicomKey key = encryptTagsVector.get(i);
        	
	    	if((tagTable.containsTag(key)) && (!(((String)tagTable.getValue(key)).trim()).equals(""))) {
	    		s = ((String)tagTable.getValue(key)).trim();
	    		digest.update(s.getBytes());
	    		byte[] encBytes = digest.digest();
	    		StringBuffer hexString = new StringBuffer();
	    		for(int k=0;k<encBytes.length;k++) {
	    			hexString.append(Integer.toHexString(0xFF & encBytes[k]));
	    		}
	    		if(enableTextArea) {
	    			outputTextArea.append("- Replacing (" + key.getKey()+ ") value of " + s + " to " + hexString  + " \n");
	    		}
	    		printStream.println("- Replacing (" + key.getKey()+ ") value of " + s + " to " + hexString);
	    		tagTable.setValue(key.getKey(), hexString);
	    	}
    	
    	}
    	
    	

    	//remove specific defined tags
    	size = removeTagsVector.size();
        for (int i = 0; i < size; i++) {
        	FileDicomKey key = removeTagsVector.get(i);
        	if(tagTable.containsTag(key)) {
        		if(enableTextArea) {
        			outputTextArea.append("- Removing (" + key.getKey()+ ") \n");
        		}
        		printStream.println("- Removing (" + key.getKey()+ ")");
        		tagTable.removeTag(key);
        	}
        }
        

        //remove private tags
    	Hashtable<FileDicomKey, FileDicomTag> fullTagsList = tagTable.getTagList();
    	Enumeration<FileDicomKey> e = fullTagsList.keys();
    	while (e.hasMoreElements()) {
            FileDicomKey tagKey = e.nextElement();
            String tag = tagKey.getKey();
    		String k = tag.substring(0, 4);
    		try {
    			int n = new Integer(k).intValue();
    			if(n%2 != 0) {
    				if(enableTextArea) {
    					outputTextArea.append("- Removing (" + tag + ") \n");
    				}
    				printStream.println("- Removing (" + tag + ")");
    	    		tagTable.removeTag(tag);
    			}
    		}catch(NumberFormatException nfe) {
    			//do nothing
    		} 
    	}
    	
    	
    	
    	//remove sequence tags
    	Enumeration<FileDicomKey> e2 = fullTagsList.keys();
    	while(e2.hasMoreElements()) {
    		FileDicomKey tagKey = e2.nextElement();
    		String tag = tagKey.getKey();
    		String type = DicomDictionary.getType(tagKey);
    		if (type == null) {
                type = "typeUnknown";
    		}
    		if (type.equals("typeSequence")) {
    			if(enableTextArea) {
    				outputTextArea.append("- Removing (" + tag + ") \n");
    			}
    			printStream.println("- Removing (" + tag + ")");
	    		tagTable.removeTag(tag);
    		}
    		if(type.equals("typeUnknown")) {
				if(tagTable.getValue(tagKey) instanceof FileDicomSQ) {
					if(enableTextArea) {
						outputTextArea.append("- Removing (" + tag + ") \n");
					}
					printStream.println("- Removing (" + tag + ")");
					tagTable.removeTag(tag);	
				}

    		}
    	}
    	
    	
    	
    	return true;
    	
    }
	
	
	
	
	
	
	/**
	 * anonymize dicom images
	 * this method does the actual anonymizing part
	 * @return
	 */
    public boolean anonymizeDICOMTags_OLD() {
    	FileDicomTagTable tagTable = fileInfoDicom.getTagTable();
    	

    	//couple of the tags (patient name (0010,0010)and patient id(0010,0020) will be replaced with a new UID which is Suject ID + DOB
    	//example: id = 1234, DOB=9/23/1968 => 1234 + 9231968 = 9233202
    	String patientID = "";
    	String dob = "";
    	String newUID = "";
    	int patientIDInt = 0;
    	
		patientID = ((String)tagTable.getValue("0010,0020")).trim();
		try {
			patientIDInt = new Integer(patientID).intValue();
		}catch(NumberFormatException e) {
			if(enableTextArea) {
				outputTextArea.append("! Patient ID(0010,0020) value is not a valid entry \n");
			}
			printStream.println("! Patient ID(0010,0020) value is not a valid entry");
			e.printStackTrace();
			return false;
		}
		dob = ((String)tagTable.getValue("0010,0030")).trim();
		if(dob.contains("/")) {
			dob = dob.replaceAll("\\/", "");
		}
		int dobInt = 0;
		try {
			dobInt = new Integer(dob).intValue();
		}catch(NumberFormatException e) {
			if(enableTextArea) {
				outputTextArea.append("! Patient DOB(0010,0030) value is not a valid entry \n");
			}
			printStream.println("! Patient DOB(0010,0030) value is not a valid entry");
			e.printStackTrace();
			return false;
		}
		int newUIDInt = patientIDInt + dobInt;
		newUID = String.valueOf(newUIDInt);

    	//one will get replaced with current date...so get current date
    	DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy");
        Date date = new Date();
        String currentDate = dateFormat.format(date);

        //one tag will get replaced with current time...so get that
        DateFormat timeFormat = new SimpleDateFormat("HHmmss");
        date = new Date();
        String currentTime = timeFormat.format(date);
        
        //bogus SPOID
        String bogusSOPID = "1.2.840.999999999999999999";
        
        //bogus implementation ID
        String bogusImplementationID =  "1.2.840.34379.17";

        //MD5
        MessageDigest digest = null;
        try {
        	digest = MessageDigest.getInstance("MD5");
        }catch(NoSuchAlgorithmException e) {
        	if(enableTextArea) {
        		outputTextArea.append("! Error in MD5 hash algorithm \n");
        	}
			printStream.println("! Error in MD5 hash algorithm");
        	e.printStackTrace();
        	return false;
        }

        //patient's birthdate (0010,0030) will be replaced with patients age...so get patients age
        String patientsAge = "";
        if(tagTable.get("0010,1010") != null) {
        	patientsAge = (String)tagTable.getValue("0010,1010");
        }
        
        //Study Instance UID (0020,000D) and Series Instance UID (0002,000E) need MIPAV version and time in milliseconds
        String studyInstanceUID = "";
        String seriesInstanceUID = "";
        String mipavVersion = MipavUtil.getVersion();
        mipavVersion = mipavVersion.replaceAll("\\.", "");
        long time = date.getTime();
    	studyInstanceUID = "1.2.840.9999.9." + mipavVersion + "." + time + ".0";
    	seriesInstanceUID = "1.2.840.9999.9." + mipavVersion + "." + time + ".1";
        
    	
        
    	if(tagTable.get("0002,0003") != null) {
    		String s = (String)tagTable.getValue("0002,0003");
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (0002,0003) value of " + s + " to " + bogusSOPID  + " \n");
    		}
    		printStream.println("- Replacing (0002,0003) value of " + s + " to " + bogusSOPID);
    		tagTable.setValue("0002,0003", bogusSOPID);
    	}
    	if(tagTable.get("0002,0012") != null) {
    		String s = (String)tagTable.getValue("0002,0012");
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (0002,0012) value of " + s + " to " + bogusImplementationID  + " \n");
    		}
    		printStream.println("- Replacing (0002,0012) value of " + s + " to " + bogusImplementationID);
    		tagTable.setValue("0002,0012", bogusImplementationID);
    	}
    	if(tagTable.get("0008,0012") != null) {
    		String s = (String)tagTable.getValue("0008,0012");
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (0008,0012) value of " + s + " to " + currentDate  + " \n");
    		}
    		printStream.println("- Replacing (0008,0012) value of " + s + " to " + currentDate);
    		tagTable.setValue("0008,0012", currentDate);
    	}
    	if(tagTable.get("0008,0013") != null) {
    		String s = (String)tagTable.getValue("0008,0013");
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (0008,0013) value of " + s + " to " + currentTime  + " \n");
    		}
    		printStream.println("- Replacing (0008,0013) value of " + s + " to " + currentTime);
    		tagTable.setValue("0008,0013", currentTime);
    	}
    	if(tagTable.get("0008,0014") != null) {
    		String s = (String)tagTable.getValue("0008,0014");
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (0008,0014) value of " + s + " to " + bogusImplementationID  + " \n");
    		}
    		printStream.println("- Replacing (0008,0014) value of " + s + " to " + bogusImplementationID);
    		tagTable.setValue("0008,0014", bogusImplementationID);
    	}
    	if(tagTable.get("0008,0018") != null) {
    		String s = (String)tagTable.getValue("0008,0018");
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (0008,0018) value of " + s + " to " + bogusSOPID  + " \n");
    		}
    		printStream.println("- Replacing (0008,0018) value of " + s + " to " + bogusSOPID);
    		tagTable.setValue("0008,0018", bogusSOPID);
    	}
    	if(tagTable.get("0008,0020") != null) {
    		String s = (String)tagTable.getValue("0008,0020");
    		String k = s.substring(0, s.lastIndexOf("/")+1) + "1000";
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (0008,0020) value of " + s + " to " + k  + " \n");
    		}
    		printStream.println("- Replacing (0008,0020) value of " + s + " to " + k );
    		tagTable.setValue("0008,0020", k);
    	}
    	if(tagTable.get("0008,0021") != null) {
    		if(enableTextArea) {
    			outputTextArea.append("- Removing (0008,0021) \n");
    		}
    		printStream.println("- Removing (0008,0021)");
    		tagTable.removeTag("0008,0021");
    	}
    	if(tagTable.get("0008,0022") != null) {
    		if(enableTextArea) {
    			outputTextArea.append("- Removing (0008,0022) \n");
    		}
    		printStream.println("- Removing (0008,0022)");
    		tagTable.removeTag("0008,0022");
    	}
    	if(tagTable.get("0008,0023") != null) {
    		if(enableTextArea) {
    			outputTextArea.append("- Removing (0008,0023) \n");
    		}
    		printStream.println("- Removing (0008,0023)");
    		tagTable.removeTag("0008,0023");
    	}
    	if((tagTable.get("0008,0050") != null) && (!(((String)tagTable.getValue("0008,0050")).trim()).equals(""))) {
    		String s = (String)tagTable.getValue("0008,0050");
    		digest.update(s.getBytes());
    		byte[] encBytes = digest.digest();
    		StringBuffer hexString = new StringBuffer();
    		for(int i=0;i<encBytes.length;i++) {
    			hexString.append(Integer.toHexString(0xFF & encBytes[i]));
    		}
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (0008,0050) value of " + s + " to " + hexString  + " \n");
    		}
    		printStream.println("- Replacing (0008,0050) value of " + s + " to " + hexString);
    		tagTable.setValue("0008,0050", hexString);
    	}
    	if((tagTable.get("0008,0080") != null) && (!(((String)tagTable.getValue("0008,0080")).trim()).equals(""))) {
    		String s = (String)tagTable.getValue("0008,0080");
    		digest.update(s.getBytes());
    		byte[] encBytes = digest.digest();
    		StringBuffer hexString = new StringBuffer();
    		for(int i=0;i<encBytes.length;i++) {
    			hexString.append(Integer.toHexString(0xFF & encBytes[i]));
    		}
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (0008,0080) value of " + s + " to " + hexString  + " \n");
    		}
    		printStream.println("- Replacing (0008,0080) value of " + s + " to " + hexString);
    		tagTable.setValue("0008,0080", hexString);
    	}
    	if(tagTable.get("0008,0081") != null) {
    		if(enableTextArea) {
    			outputTextArea.append("- Removing (0008,0081) \n");
    		}
    		printStream.println("- Removing (0008,0081");
    		tagTable.removeTag("0008,0081");
    	}
    	if((tagTable.get("0008,0090") != null) && (!(((String)tagTable.getValue("0008,0090")).trim()).equals(""))) {
    		String s = (String)tagTable.getValue("0008,0090");
    		digest.update(s.getBytes());
    		byte[] encBytes = digest.digest();
    		StringBuffer hexString = new StringBuffer();
    		for(int i=0;i<encBytes.length;i++) {
    			hexString.append(Integer.toHexString(0xFF & encBytes[i]));
    		}
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (0008,0090) value of " + s + " to " + hexString  + " \n");
    		}
    		printStream.println("- Replacing (0008,0090) value of " + s + " to " + hexString);
    		tagTable.setValue("0008,0090", hexString);
    	}
    	if(tagTable.get("0008,0092") != null) {
    		if(enableTextArea) {
    			outputTextArea.append("- Removing (0008,0092) \n");
    		}
    		printStream.println("- Removing (0008,0092)");
    		tagTable.removeTag("0008,0092");
    	}
    	if(tagTable.get("0008,0094") != null) {
    		if(enableTextArea) {
    			outputTextArea.append("- Removing (0008,0094) \n");
    		}
    		printStream.println("- Removing (0008,0094)");
    		tagTable.removeTag("0008,0094");
    	}
    	if((tagTable.get("0008,1010") != null) && (!(((String)tagTable.getValue("0008,1010")).trim()).equals(""))) {
    		String s = (String)tagTable.getValue("0008,1010");
    		digest.update(s.getBytes());
    		byte[] encBytes = digest.digest();
    		StringBuffer hexString = new StringBuffer();
    		for(int i=0;i<encBytes.length;i++) {
    			hexString.append(Integer.toHexString(0xFF & encBytes[i]));
    		}
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (0008,1010) value of " + s + " to " + hexString  + " \n");
    		}
    		printStream.println("- Replacing (0008,1010) value of " + s + " to " + hexString);
    		tagTable.setValue("0008,1010", hexString);
    	}
    	if((tagTable.get("0008,1040") != null) && (!(((String)tagTable.getValue("0008,1040")).trim()).equals(""))) {
    		String s = (String)tagTable.getValue("0008,1040");
    		digest.update(s.getBytes());
    		byte[] encBytes = digest.digest();
    		StringBuffer hexString = new StringBuffer();
    		for(int i=0;i<encBytes.length;i++) {
    			hexString.append(Integer.toHexString(0xFF & encBytes[i]));
    		}
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (0008,1040) value of " + s + " to " + hexString  + " \n");
    		}
    		printStream.println("- Replacing (0008,1040) value of " + s + " to " + hexString);
    		tagTable.setValue("0008,1040", hexString);
    	}
    	if((tagTable.get("0008,1048") != null) && (!(((String)tagTable.getValue("0008,1048")).trim()).equals(""))) {
    		String s = (String)tagTable.getValue("0008,1048");
    		digest.update(s.getBytes());
    		byte[] encBytes = digest.digest();
    		StringBuffer hexString = new StringBuffer();
    		for(int i=0;i<encBytes.length;i++) {
    			hexString.append(Integer.toHexString(0xFF & encBytes[i]));
    		}
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (0008,1048) value of " + s + " to " + hexString  + " \n");
    		}
    		printStream.println("- Replacing (0008,1048) value of " + s + " to " + hexString);
    		tagTable.setValue("0008,1048", hexString);
    	}
    	if((tagTable.get("0008,1050") != null) && (!(((String)tagTable.getValue("0008,1050")).trim()).equals(""))) {
    		String s = (String)tagTable.getValue("0008,1050");
    		digest.update(s.getBytes());
    		byte[] encBytes = digest.digest();
    		StringBuffer hexString = new StringBuffer();
    		for(int i=0;i<encBytes.length;i++) {
    			hexString.append(Integer.toHexString(0xFF & encBytes[i]));
    		}
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (0008,1050) value of " + s + " to " + hexString  + " \n");
    		}
    		printStream.println("- Replacing (0008,1050) value of " + s + " to " + hexString);
    		tagTable.setValue("0008,1050", hexString);
    	}
    	if((tagTable.get("0008,1060") != null) && (!(((String)tagTable.getValue("0008,1060")).trim()).equals(""))) {
    		String s = (String)tagTable.getValue("0008,1060");
    		digest.update(s.getBytes());
    		byte[] encBytes = digest.digest();
    		StringBuffer hexString = new StringBuffer();
    		for(int i=0;i<encBytes.length;i++) {
    			hexString.append(Integer.toHexString(0xFF & encBytes[i]));
    		}
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (0008,1060) value of " + s + " to " + hexString  + " \n");
    		}
    		printStream.println("- Replacing (0008,1060) value of " + s + " to " + hexString);
    		tagTable.setValue("0008,1060", hexString);
    	}
    	if((tagTable.get("0008,1070") != null) && (!(((String)tagTable.getValue("0008,1070")).trim()).equals(""))) {
    		String s = (String)tagTable.getValue("0008,1070");
    		digest.update(s.getBytes());
    		byte[] encBytes = digest.digest();
    		StringBuffer hexString = new StringBuffer();
    		for(int i=0;i<encBytes.length;i++) {
    			hexString.append(Integer.toHexString(0xFF & encBytes[i]));
    		}
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (0008,1070) value of " + s + " to " + hexString  + " \n");
    		}
    		printStream.println("- Replacing (0008,1070) value of " + s + " to " + hexString);
    		tagTable.setValue("0008,1070", hexString);
    	}
    	if(tagTable.get("0008,1155") != null) {
    		if(enableTextArea) {
    			outputTextArea.append("- Removing (0008,1155) \n");
    		}
    		printStream.println("- Removing (0008,1155)");
    		tagTable.removeTag("0008,1155");
    	}
    	if(tagTable.get("0010,0010") != null) {
    		String s = (String)tagTable.getValue("0010,0010");
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (0010,0010) value of " + s + " to " + newUID  + " \n");
    		}
    		printStream.println("- Replacing (0010,0010) value of " + s + " to " + newUID);
    		tagTable.setValue("0010,0010", newUID);
    	}
    	if(tagTable.get("0010,0020") != null) {
    		String s = (String)tagTable.getValue("0010,0020");
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (0010,0020) value of " + s + " to " + newUID  + " \n");
    		}
    		printStream.println("- Replacing (0010,0020) value of " + s + " to " + newUID);
    		tagTable.setValue("0010,0020", newUID);
    	}
    	if(tagTable.get("0010,0030") != null) {
    		String s = (String)tagTable.getValue("0010,0030");
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (0010,0030) value of " + s + " to " + patientsAge  + " \n");
    		}
    		printStream.println("- Replacing (0010,0030) value of " + s + " to " + patientsAge);
    		tagTable.setValue("0010,0030", patientsAge);
    	}
    	if(tagTable.get("0010,0032") != null) {
    		if(enableTextArea) {
    			outputTextArea.append("- Removing (0010,0032) \n");
    		}
    		printStream.println("- Removing (0010,0032)");
    		tagTable.removeTag("0010,0032");
    	}
    	if(tagTable.get("0010,1000") != null) {
    		if(enableTextArea) {
    			outputTextArea.append("- Removing (0010,1000) \n");
    		}
    		printStream.println("- Removing (0010,1000)");
    		tagTable.removeTag("0010,1000");
    	}
    	if(tagTable.get("0010,1001") != null) {
    		if(enableTextArea) {
    			outputTextArea.append("- Removing (0010,1001) \n");
    		}
    		printStream.println("- Removing (0010,1001)");
    		tagTable.removeTag("0010,1001");
    	}
    	if((tagTable.get("0010,1090") != null) && (!(((String)tagTable.getValue("0010,1090")).trim()).equals(""))) {
    		String s = (String)tagTable.getValue("0010,1090");
    		digest.update(s.getBytes());
    		byte[] encBytes = digest.digest();
    		StringBuffer hexString = new StringBuffer();
    		for(int i=0;i<encBytes.length;i++) {
    			hexString.append(Integer.toHexString(0xFF & encBytes[i]));
    		}
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (0010,1090) value of " + s + " to " + hexString  + " \n");
    		}
    		printStream.println("- Replacing (0010,1090) value of " + s + " to " + hexString);
    		tagTable.setValue("0010,1090", hexString);
    	}
    	if(tagTable.get("0010,2180") != null) {
    		if(enableTextArea) {
    			outputTextArea.append("- Removing (0010,2180) \n");
    		}
    		printStream.println("- Removing (0010,2180)");
    		tagTable.removeTag("0010,2180");
    	}
    	if(tagTable.get("0010,21B0") != null) {
    		if(enableTextArea) {
    			outputTextArea.append("- Removing (0010,21B0) \n");
    		}
    		printStream.println("- Removing (0010,21B0)");
    		tagTable.removeTag("0010,21B0");
    	}
    	if(tagTable.get("0010,4000") != null) {
    		if(enableTextArea) {
    			outputTextArea.append("- Removing (0010,4000) \n");
    		}
    		printStream.println("- Removing (0010,4000)");
    		tagTable.removeTag("0010,4000");
    	}
    	if((tagTable.get("0018,1000") != null) && (!(((String)tagTable.getValue("0018,1000")).trim()).equals(""))) {
    		String s = (String)tagTable.getValue("0018,1000");
    		digest.update(s.getBytes());
    		byte[] encBytes = digest.digest();
    		StringBuffer hexString = new StringBuffer();
    		for(int i=0;i<encBytes.length;i++) {
    			hexString.append(Integer.toHexString(0xFF & encBytes[i]));
    		}
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (0018,1000) value of " + s + " to " + hexString  + " \n");
    		}
    		printStream.println("- Replacing (0018,1000) value of " + s + " to " + hexString);
    		tagTable.setValue("0018,1000", hexString);
    	}
    	if(tagTable.get("0020,000D") != null) {
    		String s = (String)tagTable.getValue("0020,000D");
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (0020,000D) value of " + s + " to " + studyInstanceUID  + " \n");
    		}
    		printStream.println("- Replacing (0020,000D) value of " + s + " to " + studyInstanceUID);
    		tagTable.setValue("0020,000D", studyInstanceUID);
    	}
    	if(tagTable.get("0020,000E") != null) {
    		String s = (String)tagTable.getValue("0020,000E");
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (0020,000E) value of " + s + " to " + seriesInstanceUID  + " \n");
    		}
    		printStream.println("- Replacing (0020,000E) value of " + s + " to " + seriesInstanceUID);
    		tagTable.setValue("0020,000E", seriesInstanceUID);
    	}
    	if((tagTable.get("0020,0052") != null) && (!(((String)tagTable.getValue("0020,0052")).trim()).equals(""))) {
    		String s = (String)tagTable.getValue("0020,0052");
    		int x = s.lastIndexOf(".");
    		if(x != -1) {
    			String k = s.substring(0, x);
    			s = k + ".99999";
    		}else {
    			//need to figure out what to do here
    			//for now...just seet it to 99999
    			s = "99999";
    		}
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (0020,0052) value of " + s + " to " + s  + " \n");
    		}
    		printStream.println("- Replacing (0020,0052) value of " + s + " to " + s);
    		tagTable.setValue("0020,0052", s);
    	}
    	if((tagTable.get("0020,0200") != null) && (!(((String)tagTable.getValue("0020,0200")).trim()).equals(""))) {
    		String s = (String)tagTable.getValue("0020,0200");
    		int x = s.lastIndexOf(".");
    		if(x != -1) {
    			String k = s.substring(0, x);
    			s = k + ".99999";
    		}else {
    			//need to figure out what to do here
    			//for now...just seet it to 99999
    			s = "99999";
    		}
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (0020,0200) value of " + s + " to " + s  + " \n");
    		}
    		printStream.println("- Replacing (0020,0200) value of " + s + " to " + s);
    		tagTable.setValue("0020,0200", s);
    	}
    	if(tagTable.get("0040,0275") != null) {
    		if(enableTextArea) {
    			outputTextArea.append("- Removing (0040,0275) \n");
    		}
    		printStream.println("- Removing (0040,0275)");
    		tagTable.removeTag("0040,0275");
    	}
    	if((tagTable.get("0040,A124") != null) && (!(((String)tagTable.getValue("0040,A124")).trim()).equals(""))) {
    		String s = (String)tagTable.getValue("0040,A124");
    		int x = s.lastIndexOf(".");
    		if(x != -1) {
    			String k = s.substring(0, x);
    			s = k + ".99999";
    		}else {
    			//need to figure out what to do here
    			//for now...just seet it to 99999
    			s = "99999";
    		}
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (0040,A124) value of " + s + " to " + s  + " \n");
    		}
    		printStream.println("- Replacing (0040,A124) value of " + s + " to " + s);
    		tagTable.setValue("0040,A124", s);
    	}
    	if(tagTable.get("0040,A730") != null) {
    		if(enableTextArea) {
    			outputTextArea.append("- Removing (0040,A730) \n");
    		}
    		printStream.println("- Removing (0040,A730)");
    		tagTable.removeTag("0040,A730");
    	}
    	if((tagTable.get("0088,0140") != null) && (!(((String)tagTable.getValue("0088,0140")).trim()).equals(""))) {
    		String s = (String)tagTable.getValue("0088,0140");
    		int x = s.lastIndexOf(".");
    		if(x != -1) {
    			String k = s.substring(0, x);
    			s = k + ".99999";
    		}else {
    			//need to figure out what to do here
    			//for now...just seet it to 99999
    			s = "99999";
    		}
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (0088,0140) value of " + s + " to " + s  + " \n");
    		}
    		printStream.println("- Replacing (0088,0140) value of " + s + " to " + s);
    		tagTable.setValue("0088,0140", s);
    	}
    	if((tagTable.get("3006,0024") != null) && (!(((String)tagTable.getValue("3006,0024")).trim()).equals(""))) {
    		String s = (String)tagTable.getValue("3006,0024");
    		int x = s.lastIndexOf(".");
    		if(x != -1) {
    			String k = s.substring(0, x);
    			s = k + ".99999";
    		}else {
    			//need to figure out what to do here
    			//for now...just seet it to 99999
    			s = "99999";
    		}
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (3006,0024) value of " + s + " to " + s  + " \n");
    		}
    		printStream.println("- Replacing (3006,0024) value of " + s + " to " + s);
    		tagTable.setValue("3006,0024", s);
    	}
    	if((tagTable.get("3006,00C2") != null) && (!(((String)tagTable.getValue("3006,00C2")).trim()).equals(""))) {
    		String s = (String)tagTable.getValue("3006,00C2");
    		int x = s.lastIndexOf(".");
    		if(x != -1) {
    			String k = s.substring(0, x);
    			s = k + ".99999";
    		}else {
    			//need to figure out what to do here
    			//for now...just seet it to 99999
    			s = "99999";
    		}
    		if(enableTextArea) {
    			outputTextArea.append("- Replacing (3006,00C2) value of " + s + " to " + s  + " \n");
    		}
    		printStream.println("- Replacing (3006,00C2) value of " + s + " to " + s);
    		tagTable.setValue("3006,00C2", s);
    	}

    	//remove private tags
    	Hashtable<FileDicomKey, FileDicomTag> fullTagsList = tagTable.getTagList();
    	Enumeration<FileDicomKey> e = fullTagsList.keys();
    	while (e.hasMoreElements()) {
            FileDicomKey tagKey = e.nextElement();
            String tag = tagKey.getKey();
    		String k = tag.substring(0, 4);
    		try {
    			int n = new Integer(k).intValue();
    			if(n%2 != 0) {
    				if(enableTextArea) {
    					outputTextArea.append("- Removing (" + tag + ") \n");
    				}
    				printStream.println("- Removing (" + tag + ")");
    	    		tagTable.removeTag(tag);
    			}
    		}catch(NumberFormatException nfe) {
    			//do nothing
    		} 
    	}
    	
    	//remove sequence tags
    	Enumeration<FileDicomKey> e2 = fullTagsList.keys();
    	while(e2.hasMoreElements()) {
    		FileDicomKey tagKey = e2.nextElement();
    		String tag = tagKey.getKey();
    		String type = DicomDictionary.getType(tagKey);
    		if (type == null) {
                type = "typeUnknown";
    		}
    		if (type.equals("typeSequence")) {
    			if(enableTextArea) {
    				outputTextArea.append("- Removing (" + tag + ") \n");
    			}
    			printStream.println("- Removing (" + tag + ")");
	    		tagTable.removeTag(tag);
    		}
    		if(type.equals("typeUnknown")) {
				if(tagTable.getValue(tagKey) instanceof FileDicomSQ) {
					if(enableTextArea) {
						outputTextArea.append("- Removing (" + tag + ") \n");
					}
					printStream.println("- Removing (" + tag + ")");
					tagTable.removeTag(tag);	
				}

    		}
    	}

    	return true;
    }
    
    public boolean delete(File file) {
    	if(file.isDirectory()) {
    		File[] children = file.listFiles();
    		for(int i=0;i<children.length;i++) {
    			if(children[i].isDirectory()) {
    				boolean success = delete(children[i]);
    				if(!success) {
    					return false;
    				}
    			}else {
    				return false;
    			}

    		}
    	}else {
    		return false;
    	}
    	
    	return file.delete();
    }
	
    /**
     * finalize
     */
	public void finalize() {
		if(inputImage != null) {
			inputImage.disposeLocal();
			inputImage = null;
		}
		try{
			if(outputStream != null) {
				outputStream.close();
			}
		}catch(Exception e) {
			
		}
	}


	/**
	 * get output text filename
	 * @return
	 */
	public String getOutputTextFileName() {
		return outputTextFileName;
	}


	/**
	 * get output directory path
	 * @return
	 */
	public String getInputDirectoryPath() {
		return inputDirectoryPath;
	}


	/**
	 * set alg canceled
	 * @param algCanceled
	 */
	public void setAlgCanceled(boolean algCanceled) {
		this.algCanceled = algCanceled;
	}
	
	
	
	
	
	

}
