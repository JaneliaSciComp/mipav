package gov.nih.mipav.model.provenance;


import gov.nih.mipav.model.scripting.actions.ActionBase;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.view.dialogs.AlgorithmParameters;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

import java.util.Vector;


/**
 * A singleton class used to add lines to the script currently being recorded (if one is being recorded at all).
 */
public class ProvenanceRecorder {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** The line separator to use for scripts. */
    protected static final String LINE_SEPARATOR = "\n";

    /** The reference to the only occurrance of this class. */
    protected static ProvenanceRecorder singletonReference = null;
    
    /** Status indicating that the script recorder is currently paused. */
    public static final int PAUSED = 2;
    
    /** Status indicating that the script recorder is currently recording. */
    public static final int RECORDING = 1;
    
    /** Status indicating that the script recorder is currently stopped (either never started, or recording is over). */
    public static final int STOPPED = 0;
    
    /** Strings describing the various recorder statuses. */
    public static final String[] statusStrings = {"Stopped", "Recording", "Paused"};

    //~ Instance fields ------------------------------------------------------------------------------------------------
 
    
    /** The current status of the recorder, either PAUSED, RECORDING, or STOPPED. */
    protected int recorderStatus;
    
    /** A list of listeners who want to know about lines added to the script. */
    protected Vector recordingListeners = new Vector();
    
    /** The table containing image-placeholder-to-image-name mappings for the script we are currently recording. */
    protected ImageVariableTable imageTable;

    protected Vector<String> inputImages;
    protected Vector<String> outputImages;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ProvenanceRecorder object.  May only be called by <code>getReference()</code> since this is a singleton class.
     */
    protected ProvenanceRecorder() {
        imageTable = new ImageVariableTable();
        inputImages = new Vector<String>();
        outputImages = new Vector<String>();
        recorderStatus = STOPPED;
        Preferences.debug("script recorder:\tCreated" + "\n", Preferences.DEBUG_SCRIPTING);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Returns a reference to the script recorder.
     *
     * @return  A reference to the script recorder.
     */
    public synchronized static final ProvenanceRecorder getReference() {

        if (singletonReference == null) {
            singletonReference = new ProvenanceRecorder();
        }

        return singletonReference;
    }
    
    public synchronized void addInputImage(String var) {
    	inputImages.addElement(var);
    }
    
    public synchronized void addOutputImage(String var) {
    	outputImages.addElement(var);
    }
    
    /**
     * Add a line to the current script by requesting a script action to generate its script line.
     * @param  scriptAction  The script action to add to the current script we are recording.
     */
    public synchronized void addLine(ScriptableActionInterface scriptAction) {
    	//set this to be recognized as a provenance action (not script)
    	if (scriptAction instanceof ActionBase) {
    		((ActionBase)scriptAction).setIsScript(false);
    	}
    	
        if (getRecorderStatus() == RECORDING) {
        	System.err.println("here");
            scriptAction.insertScriptLine();
            // no notification is necessary since the scriptAction must call the other addLine() method to actually add to the script
        }
    }

    /**
     * Appends a line of text to the current script (if one is being recorded) using a script action and list of parameters.
     *
     * @param  action         The action to put into the new script line.
     * @param  parameterList  The list of parameters to include in the new script line.
     */
    public synchronized void addLine(String action, ParameterTable parameterList) {
        if (getRecorderStatus() == RECORDING) {
        	System.err.println("in addLine(action, plist)");
        	//create new DataProvenanceEntry
        	//   add entry to the correct
        	String entryString = action + "(" + parameterList.convertToDPString(imageTable) + ")" + LINE_SEPARATOR;
            
            ProvenanceEntry entry = new ProvenanceEntry(entryString, System.currentTimeMillis());
                       
            boolean doOutput = false;
            
            try {
            	doOutput = parameterList.getBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE);
            	
            } catch (ParameterException e) { }
            
            System.err.println("Do output: " + doOutput + ", inputimages size: " + inputImages.size() + ", outputimages size: " + outputImages.size());
            
           if (doOutput) {                	
                	//output image created... get provenance from srcImage, put that into output image,
                	//  and add the new entry
                	
        	   for (int i = 0; i < outputImages.size(); i++) {
        		   imageTable.getImage(outputImages.elementAt(i)).getProvenanceHolder().addElement(entry);
        		   imageTable.getImage(outputImages.elementAt(i)).getProvenanceHolder().sort();
        		   
        		   System.err.println("Output Provenance: " + imageTable.getImage(outputImages.elementAt(i)).getProvenanceHolder() + "\n");
        		   
        	   }
           } 
           else {
//            	add the new entry to the srcImage(s) provenance
            	
        	   for (int i = 0; i < inputImages.size(); i++) {
        		   imageTable.getImage(inputImages.elementAt(i)).getProvenanceHolder().addElement(entry);
        		   imageTable.getImage(inputImages.elementAt(i)).getProvenanceHolder().sort();
        	   }
           }      	
            
           
           
            //clear out the vectors holding input and output registers            
           inputImages.clear();
           outputImages.clear();
            
        }
    }

    /**
     * Erases the current script.
     */
    public synchronized void resetScript() {
        imageTable = new ImageVariableTable();
        
        Preferences.debug("script recorder:\tScript reset" + "\n", Preferences.DEBUG_SCRIPTING);
    }
    
    /**
     * Returns a reference to the image table being used to run the current script.
     * 
     * @return  The image table which should be used to run the current script, or <code>null</code> if no script is being run at the moment.
     */
    public synchronized ImageVariableTable getImageTable() {
        if (getRecorderStatus() == RECORDING) {
            return imageTable;
        }
        
        Preferences.debug("script recorder:\tRetrieved image table while no script is being recorded." + "\n", Preferences.DEBUG_SCRIPTING);
        return null;
    }

    /**
     * Returns the current status of the script recorder.
     * 
     * @return  Either PAUSED, RECORDING, or STOPPED.
     */
    public synchronized int getRecorderStatus() {
        return recorderStatus;
    }

    /**
     * Changes the current status of the script recorder and notifies any recorder listeners of the change.
     * 
     * @param  status  The new status for the script recorder.
     */
    protected synchronized void setRecorderStatus(int status) {
        Preferences.debug("provenance recorder:\tStatus changed from\t" + statusStrings[recorderStatus] + "\tto\t" + statusStrings[status] + "\n", Preferences.DEBUG_SCRIPTING);
        
        recorderStatus = status;
    }
        
    /**
     * Starts the recording of a new script (if one isn't already being recorded).  Any script text which was 
     * previously recorded and stored in the recorder is erased (unless we are resuming from a recorder pause).
     * 
     * @return  <code>True</code> if recording was started successfully, <code>false</code> otherwise.
     */
    public synchronized boolean startRecording() {
        if (getRecorderStatus() == STOPPED) {
            Preferences.debug("provenance recorder:\tNew script started." + "\n", Preferences.DEBUG_SCRIPTING);
            resetScript();
            setRecorderStatus(RECORDING);
            return true;
        } else if (getRecorderStatus() == PAUSED) {
            Preferences.debug("provenance recorder:\tresumed." + "\n", Preferences.DEBUG_SCRIPTING);
            setRecorderStatus(RECORDING);
            return true;
        } else {
            MipavUtil.displayError("provenance recording startup failed.  Cannot record two scripts at the same time.");
            return false;
        }
    }
    
    /**
     * Pauses the recording of the script we are currently recording.
     */
    public synchronized void pauseRecording() {
        if (getRecorderStatus() == RECORDING) {
            Preferences.debug("script recorder:\tPaused." + "\n", Preferences.DEBUG_SCRIPTING);
            setRecorderStatus(PAUSED);
        } else {
            MipavUtil.displayError("Cannot pause script recording.  Recording is either already paused or is stopped.");
            return;
        }
    }
    
    /**
     * Stops script recording.  Call <code>getScript()</code> to get the recorded script.
     */
    public synchronized void stopRecording() {
        if (getRecorderStatus() != STOPPED) {
            Preferences.debug("script recorder:\tStopped." + "\n", Preferences.DEBUG_SCRIPTING);
            setRecorderStatus(STOPPED);
        } else {
            MipavUtil.displayError("Cannot stop script recording.  No recording is currently taking place.");
            return;
        }
    }
    
    /**
     * Convenience method used to store the name of an image in the image table being recorded/used in the current script.
     * 
     * @param   imageName  The name of the image to store.
     * 
     * @return  The image variable placeholder which has been assigned to the image name (may not be a new variable if the name is already in the table).
     */
    public synchronized String storeImage(String imageName) {
    	System.err.println("Calling storeImage: " + imageName);
    	
    	return getImageTable().storeImageName(imageName);
    }
   
}
