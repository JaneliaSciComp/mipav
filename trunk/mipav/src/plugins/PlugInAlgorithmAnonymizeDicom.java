import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileDicom;


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
				ReadDicom imageFile = new ReadDicom(selectedFiles[i].getName(), selectedFiles[i].getAbsolutePath());
	            imageFile.setQuiet(true); // if we want quiet, we tell the reader, too.
	            imageFile.readHeader(true); // can we read the header?
            } catch(IOException ioe) {
            	ioe.printStackTrace();
            }
		}
		
	}
	
	private class ReadDicom extends FileDicom {

		public ReadDicom(String name, String dir) throws IOException {
			super(name, dir);
		}
		
		public boolean readHeader(boolean loadTagBuffer) throws IOException {
	        return super.readHeader(loadTagBuffer);
	    }

	}
	

}
