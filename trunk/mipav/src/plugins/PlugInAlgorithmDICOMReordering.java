import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.TreeSet;

import javax.swing.JTextArea;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileDicom;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.Preferences;

/**
 * @author pandyan
 * 
 * The plugin is used for sorting DICOM image slices based on the
 * patient location <0020,0037>
 * 
 * New dirs are created using patientID info as well as studyID and series...as well as whether the initial images were
 * in a "pre" or "post" folder
 *
 */
public class PlugInAlgorithmDICOMReordering extends AlgorithmBase {
	/** study path **/
	private String studyPath;
	
	/** parent dir of the study dir...new dirs are created under this **/
	private String parentDir;
	
	/** boolean for test **/
	private boolean success;
	
	/** this is a list of volumeTreeMap objects **/
	private ArrayList<TreeSet<String[]>> volumes = new ArrayList<TreeSet<String[]>>();
	
	/** number of images in dir **/
	private int numImages = 0;
	
	/** handle to textarea **/
	private JTextArea outputTextArea;

	
	/**
	 * 
	 *
	 */
	public PlugInAlgorithmDICOMReordering() {
		
	}
	
	/**
	 * constructor
	 * @param studyPath
	 */
	public PlugInAlgorithmDICOMReordering(String studyPath,JTextArea outputTextArea) {
		this.studyPath = studyPath;
		this.outputTextArea = outputTextArea;
		
		
	}
	

	/**
	 *  run algorithm
	 */
	public void runAlgorithm() {
		//long begTime = System.currentTimeMillis();


        if (outputTextArea != null) {
            outputTextArea.append("** Beginning Algorithm \n");
        }
        
		//remove last slash from study path if it has it
        if(String.valueOf(studyPath.charAt(studyPath.length() - 1)).equals(File.separator)) {
        	studyPath = studyPath.substring(0,studyPath.length() - 1);
	    }
        
        parentDir = studyPath.substring(0, studyPath.lastIndexOf(File.separator));

        // first create a File object based upon the study path
        File studyPathRoot = new File(studyPath);
        
        try {
        	if (outputTextArea != null) {
                outputTextArea.append("* Parsing");
            }
            success = parse(studyPathRoot);
            if(success == false) {
            	if (outputTextArea != null) {
                    outputTextArea.append("\n");
                }
            	setCompleted(true);
                return;
            }
            
            if (outputTextArea != null) {
                outputTextArea.append("\n* Number of image slices in dir is " + numImages + " \n");
            }
            
            
            if (outputTextArea != null) {
                outputTextArea.append("* Copying files");
            }
            success = copyFiles();
            if(success == false) {
            	if (outputTextArea != null) {
                    outputTextArea.append("\n");
                }
            	setCompleted(true);
                return;
            }
            
            
        } catch (Exception e) {
        	if (outputTextArea != null) {
                outputTextArea.append("\n! ERROR: exception....exiting algorithm \n");
            }
            e.printStackTrace();
            finalize();
            setCompleted(true);
            return;
        }
        
        if (outputTextArea != null) {
            outputTextArea.append("\n** Ending Algorithm \n");
        }

        setCompleted(true);

	}

	
	/**
	 * This method parses the dir
	 * It drills down to each image slice, making dirs on the way if needed.
	 * when it gets to the image slice, info is extracted and sorted collections are built
	 * These collections are then used when copying the files over
	 * 
	 * 
	 * @param file
	 * @return
	 * @throws IOException
	 * @throws OutOfMemoryError
	 */
	 public boolean parse(File file) throws IOException, OutOfMemoryError {
		 	File[] children = file.listFiles();
	        FileDicom imageFile = null;
	        File test;

	        try {

	            for (int i = 0; i < children.length; i++) {

	                if (isThreadStopped()) {
	                    return false;
	                }

	                if (children[i].isDirectory()) {
	                    parse(children[i]);
	                } else if (!children[i].isDirectory()) {
	                	if ((numImages % 20) == 0) {
	                        if (outputTextArea != null) {
	                            outputTextArea.append(".");
	                        }
	                    }
	                	String prePostDir = children[i].getParent();
	                	String prePost = prePostDir.substring(prePostDir.lastIndexOf(File.separator)+1,prePostDir.length());

	                    try {

	                    	imageFile = new FileDicom(children[i].getName(), children[i].getParent() + File.separatorChar);
	                        success = imageFile.readHeader(true);

	                    } catch (IOException error) {
	                    	error.printStackTrace();
	                    	return false;

	                    }
	                    numImages++;
	                    FileInfoDicom fileInfoDicom = (FileInfoDicom) imageFile.getFileInfo();
	                    
	                    String patientID = ((String) fileInfoDicom.getTagTable().getValue("0010,0020")).trim();
	                    test = new File(parentDir + File.separator + patientID);
	                    if(!test.exists()) {
	                    	test.mkdir();
	                    }
	                    
	                    test = new File(parentDir + File.separator + patientID + File.separator + prePost);
	                    if(!test.exists()) {
	                    	test.mkdir();
	                    }
	                    
	                    String studyNo = "study" + ((String) fileInfoDicom.getTagTable().getValue("0020,0010")).trim();
	                    test = new File(parentDir + File.separator + patientID + File.separator + prePost + File.separator + studyNo);
	                    if(!test.exists()) {
	                    	test.mkdir();
	                    }
	                    
	                    String seriesNo = "series" + ((String) fileInfoDicom.getTagTable().getValue("0020,0011")).trim();
	                    test = new File(parentDir + File.separator + patientID + File.separator + prePost + File.separator + studyNo + File.separator + seriesNo);
	                    if(!test.exists()) {
	                    	test.mkdir();
	                    }

	                    TransMatrix matrix = null;
	                    float[] tPt = new float[3];
	                    matrix = fileInfoDicom.getPatientOrientation();

	                    if (matrix != null) {

	                        /* transform the x location, y location, and z location, found
	                         * from the Image Position tag, by the matrix.  The tPt array now has the three numbers arranged
	                         * as if this image had been transformed.  The third place in the array holds the number that
	                         * the axis is being sliced along. xlocation, ylocation, zlocation are from the DICOM tag
	                         * 0020,0032 patient location.....in other words tPt[2] is the slice location;
	                         */
	                        matrix.transform(fileInfoDicom.xLocation, fileInfoDicom.yLocation, fileInfoDicom.zLocation,
	                                         tPt);

	                    } else {
	                        Preferences.debug("! ERROR: unable to obtain image orientation matrix.....exiting algorithm \n",
	                                          Preferences.DEBUG_ALGORITHM);

	                        System.out.println("! ERROR: unable to obtain image orientation matrix.....exiting algorithm \n");

	                        return false;
	                    }
	                    
	                    String currentAbsPath = children[i].getAbsolutePath();
	                    String newDir = parentDir + File.separator + patientID + File.separator + prePost + File.separator + studyNo + File.separator + seriesNo;
	                    String sortingValue = String.valueOf(tPt[2]);
	                    String[] info = new String[3];
	                    info[0] = currentAbsPath;
	                    info[1] = newDir;
	                    info[2] = sortingValue;
	                    
	                    
	                    TreeSet<String[]> treeSet;
	                    boolean found = false;
	                    Iterator<String[]> iter;
	                    String[] arr;

	                    for(int k=0;k<volumes.size();k++) {
	                    	treeSet = volumes.get(k);
	                    	iter = treeSet.iterator();
	                    	arr = (String[]) iter.next();
	                    	if(arr[1].equals(newDir)) {
	                    		found = true;
	                    		treeSet.add(info);	
	                    	}
	                    }
	                    if(found == false) {
	                    	treeSet = new TreeSet<String[]>(new SortingValueComparator());
	                    	treeSet.add(info);
	                    	volumes.add(treeSet);
	                    }

	                }
	            }
	        }finally {
	        	if(imageFile != null) {
	        		imageFile.finalize();
	        		imageFile = null;
	        	}
	        }

	        return true;
	 }
	 
	 
	 /**
	  * this method copies the src file to the dest file
	  * @return
	  */
	 public boolean copyFiles() {
		 TreeSet<String[]> treeSet;
		 Iterator<String[]> iter;
		 String[] arr;
		 String currentAbsPath;
		 String newDir;
		 String newPath;
		 File srcFile;
		 File destFile;
		 int counter = 0;

		 for(int k=0;k<volumes.size();k++) {
			treeSet = volumes.get(k);
         	iter = treeSet.iterator();
         	int imageSlice = 0;
			 
         	 while (iter.hasNext()) {
         		if ((counter % 20) == 0) {
                    if (outputTextArea != null) {
                        outputTextArea.append(".");
                    }
                }
                 arr = (String[]) iter.next();
                 currentAbsPath = arr[0];
                 srcFile = new File(currentAbsPath);
                 newDir = arr[1];
                 newPath = newDir + File.separator + "image" + imageSlice;
                 destFile = new File(newPath);
                 try {
                	 copy(srcFile,destFile);
                 }
                 catch(Exception e) {
                	 e.printStackTrace();
                	 return false;
                 }
                 imageSlice++;
                 counter++;
         	 }  
         	 
		 }
		 
		 return true; 
	 }
	 
	 
	 
	 
	 
	 /**
	  * copies source file to dest file
	  * @param src
	  * @param dst
	  * @throws IOException
	  */
	 public void copy(File src, File dst) throws IOException {
        InputStream in = new FileInputStream(src);
        OutputStream out = new FileOutputStream(dst);
        // Transfer bytes from in to out
        byte[] buf = new byte[1024];
        int len;
        while ((len = in.read(buf)) > 0) {
            out.write(buf, 0, len);
        }
        in.close();
        out.close();
	 }
	    
	    
	    
	    
	    /**
	     * This inner class is used to sort the treeSet based on the sorting value
	     */
	    private class SortingValueComparator implements Comparator<Object> {

	    	/**
	         *
	         * @param   oA  
	         * @param   oB  
	         *
	         * @return  int
	         */
	        public int compare(Object oA, Object oB) {
	        	String[] aA = (String[]) oA;
	            String[] aB = (String[]) oB;
	            
	            float sortingValueA = new Float(((String) aA[2]).trim()).floatValue();
	            float sortingValueB = new Float(((String) aB[2]).trim()).floatValue();
	            
	            if (sortingValueA < sortingValueB) {
                    return -1;
                }else {
                	return 1;
                }
	        }
	    }
	                    
}
