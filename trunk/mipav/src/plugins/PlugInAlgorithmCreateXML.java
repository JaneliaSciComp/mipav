import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.view.MipavUtil;

public class PlugInAlgorithmCreateXML extends AlgorithmBase {

	// ~ Instance fields ---------------------------------------------------------------------------------------
	/** Files and folders selected by the user */	
	private File[] selectedFiles;
	
	


	//	~ Constructors -----------------------------------------------------------------------------------------
	public PlugInAlgorithmCreateXML(File[] inputFiles) {
		
		selectedFiles = inputFiles;
		
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
		
		ArrayList<Integer> filesNotRead = new ArrayList<Integer>();
		boolean isRead = false;
		for(int i=0; i<selectedFiles.length; i++) {
			// use the selectedFileName as the reference slice for the file info tag tables
            isRead = false;
			fireProgressStateChanged((int)(100*(((double)i)/((double)selectedFiles.length))), null, "Processing file "+i);
			if(selectedFiles[i].isDirectory()) {
				isRead = writeFolderXML(selectedFiles[i]);
			} else {
				isRead = writeFileXML(selectedFiles[i]);
			}
			
			if(!isRead) {
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
		
		System.out.println("Finished reading files");
		
	}
	
	/**
	 * Method for writing files to XML using dataset schema.
	 * @param f is not a directory
	 * @return
	 */
	private boolean writeFileXML(File f) {
		return false;
	}
	
	/**
	 * Method for writing folders to XML using dataset schema.
	 * @param f is a directory
	 * @return
	 */
	private boolean writeFolderXML(File f) {
		return false;
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
