import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileDicom;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.view.ViewImageFileFilter;

/**
 * 
 * @author pandyan
 *
 */
public class PlugInAlgorithmDICOMReordering extends AlgorithmBase {
	/** study path **/
	private String studyPath;
	
	private String parentDir;
	
	private boolean success;
	
	
	private int numImages = 0;
	
	private String newStudyDir, newSeriesDir, newVolumeDir, imageName;
	
	/**
	 * 
	 *
	 */
	public PlugInAlgorithmDICOMReordering() {
		
	}
	
	/**
	 * 
	 * @param studyPath
	 */
	public PlugInAlgorithmDICOMReordering(String studyPath) {
		this.studyPath = studyPath;
	}
	

	/**
	 * 
	 */
	public void runAlgorithm() {
		//remove last slash from study path if it has it
        if(String.valueOf(studyPath.charAt(studyPath.length() - 1)).equals(File.separator)) {
        	studyPath = studyPath.substring(0,studyPath.length() - 1);
	    }
        
        parentDir = studyPath.substring(0, studyPath.lastIndexOf(File.separator));

        
        
        
        // first create a File object based upon the study path
        File studyPathRoot = new File(studyPath);
        
        try {
            success = parse(studyPathRoot);
        } catch (Exception e) {
            System.out.println("! ERROR: " + e.getCause().toString() + "\n");
            finalize();
            setCompleted(true);
            return;
        }

        setCompleted(true);

	}

	
	
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
	                	String prePostDir = children[i].getParent();
	                	String prePost = prePostDir.substring(prePostDir.lastIndexOf(File.separator)+1,prePostDir.length());

	                    try {

	                    	imageFile = new FileDicom(children[i].getName(), children[i].getParent() + File.separatorChar);
	                        success = imageFile.readHeader(true);

	                    } catch (IOException error) {
	                    	error.printStackTrace();
	                    	throw error;

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

	                    String instanceNo = ((String) fileInfoDicom.getTagTable().getValue("0020,0013")).trim();
	                    String imageName = "image" + instanceNo;
	                    
	                    System.out.println(patientID + " " + studyNo + " " + seriesNo + " " + instanceNo);
	                    
	                    
	                    test = new File(parentDir + File.separator + patientID + File.separator + prePost + File.separator + studyNo + File.separator + seriesNo + File.separator + imageName);
	                    
	                    copy(children[i],test);
	                    
	                    
	                    
	                    
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
	 
	 
	 
	 	// Copies src file to dst file.
	    // If the dst file does not exist, it is created
	    void copy(File src, File dst) throws IOException {
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
	                    
}
