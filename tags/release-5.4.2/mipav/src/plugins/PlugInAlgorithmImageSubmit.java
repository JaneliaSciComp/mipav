import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJProgressBar;


public class PlugInAlgorithmImageSubmit extends AlgorithmBase implements AlgorithmInterface {

	public static final String IMAGE_BASE_LOC = "IMAGES";
	
	public static final String PATIENT_BASE_LOC = "PII";
	
	/**Text fields describing interesting parts of the xml. */
	private HashMap<String, String> textFieldsMap;
	
	/**The files comprising the data set*/
	private File[] selectedFiles;
	
	/**The DICOM tags to be anonymized*/
	private String[] tagArray;
	
	/** The location for submission of this image */
	private String submitImageLocation;
	
	/** The location of all pii relevant to the given dataset*/
	private String submitPatientLocation;
	
	/**Algorithm for anonymizing pii in images*/
	private PlugInAlgorithmAnonymizeDicom anonymizeDicom;
	
	/**Algorithm for creating image description file*/
	private PlugInAlgorithmCreateXML createXmlAlgo;
	
	/** Progress bar that will listen to a dialog's algorithm (and reflect current progress)*/
    protected ViewJProgressBar progressBar;
    
    /**The location of the xml file*/
    private ArrayList<String> xmlLoc;
    
    /**The location of the anonymization results. */
    private String anonLoc;
	
	public PlugInAlgorithmImageSubmit(File[] selectedFiles,
			HashMap<String, String> map, String submitImageLocation,
			String[] tagArray, boolean submitFiles) {
		this.textFieldsMap = map;
		this.selectedFiles = selectedFiles;
		this.tagArray = tagArray;
		this.submitImageLocation = submitImageLocation.charAt(submitImageLocation.length()-1) == File.separatorChar ? 
										submitImageLocation : submitImageLocation+File.separator;
		this.submitPatientLocation = selectedFiles[0].getParent()+File.separator+PATIENT_BASE_LOC;
		
		File f = new File(submitImageLocation);
		if(!f.exists()) {
			f.mkdirs();
		}
		
		f = new File(submitPatientLocation);
		if(!f.exists()) {
			f.mkdirs();
		}
		
		createProgressBar("Image Submission", "Initializing...");
	}
	
	/******************************************************************************
	 * @param title progress bar's title
     * @param msg the message to display on the progress bar (initial setting)
     *******************************************************************************/
    protected void createProgressBar(String title, String msg) {
    	progressBar = new ViewJProgressBar(title, msg, 0, 100, true);
        progressBar.setSeparateThread(false);
    }
	
	@Override
	public void runAlgorithm() {
		createXmlAlgo = new PlugInAlgorithmCreateXML(selectedFiles, textFieldsMap, submitImageLocation);
		anonymizeDicom = new PlugInAlgorithmAnonymizeDicom(selectedFiles, tagArray, submitImageLocation, submitPatientLocation);
		
		createXmlAlgo.addListener(this);
		anonymizeDicom.addListener(this);
		
		createXmlAlgo.addProgressChangeListener(progressBar);
		createXmlAlgo.setProgressValues(0, 50);
		
		anonymizeDicom.addProgressChangeListener(progressBar);
        anonymizeDicom.setProgressValues(50, 100);
		
		createXmlAlgo.start();
		anonymizeDicom.start();
	}

	public void algorithmPerformed(AlgorithmBase algorithm) {
		progressBar.dispose();
		if(algorithm instanceof PlugInAlgorithmAnonymizeDicom) {
	    	Preferences.debug("Anonymization algorithm complete");
	    	anonLoc = anonymizeDicom.getAnonResultsLoc();
			anonymizeDicom.finalize();
	    	anonymizeDicom = null;
		} else if(algorithm instanceof PlugInAlgorithmCreateXML) {
			Preferences.debug("Finished writing xml");
			xmlLoc = createXmlAlgo.getXmlList();
	    	
	    	createXmlAlgo.finalize();
	    	createXmlAlgo = null;
		}
		
		
		if(anonymizeDicom == null && createXmlAlgo == null) {
	    	String totalStr = "An XML file was written to: \n"+xmlLoc.get(0)+"\n\nAnonymization results were written to: \n"+
	    						anonLoc+"\n\nThe selected files were copied to: \n"+submitImageLocation;
	    	MipavUtil.displayInfo(totalStr);
		}
	}

}
