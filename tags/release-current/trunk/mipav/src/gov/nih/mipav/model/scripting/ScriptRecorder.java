package gov.nih.mipav.model.scripting;


import gov.nih.mipav.model.scripting.parameters.ParameterTable;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

import java.util.Vector;


/**
 * A singleton class used to add lines to the script currently being recorded (if one is being recorded at all).
 */
public class ScriptRecorder {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** The line separator to use for scripts. */
    protected static final String LINE_SEPARATOR = "\n";

    /** The reference to the only occurrance of this class. */
    protected static ScriptRecorder singletonReference = null;
    
    /** Status indicating that the script recorder is currently paused. */
    public static final int PAUSED = 2;
    
    /** Status indicating that the script recorder is currently recording. */
    public static final int RECORDING = 1;
    
    /** Status indicating that the script recorder is currently stopped (either never started, or recording is over). */
    public static final int STOPPED = 0;
    
    /** Strings describing the various recorder statuses. */
    public static final String[] statusStrings = {"Stopped", "Recording", "Paused"};

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The contents of the current script being recorded. */
    protected String script;
    
    /** The current status of the recorder, either PAUSED, RECORDING, or STOPPED. */
    protected int recorderStatus;
    
    /** A list of listeners who want to know about lines added to the script. */
    protected Vector recordingListeners = new Vector();
    
    /** The table containing image-placeholder-to-image-name mappings for the script we are currently recording. */
    protected ImageVariableTable imageTable;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ScriptRecorder object.  May only be called by <code>getReference()</code> since this is a singleton class.
     */
    protected ScriptRecorder() {
        script = new String();
        imageTable = new ImageVariableTable();
        recorderStatus = STOPPED;
        Preferences.debug("script recorder:\tCreated" + "\n", Preferences.DEBUG_SCRIPTING);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Returns a reference to the script recorder.
     *
     * @return  A reference to the script recorder.
     */
    public synchronized static final ScriptRecorder getReference() {

        if (singletonReference == null) {
            singletonReference = new ScriptRecorder();
        }

        return singletonReference;
    }
    
    /**
     * Add a line to the current script by requesting a script action to generate its script line.
     * @param  scriptAction  The script action to add to the current script we are recording.
     */
    public synchronized void addLine(ScriptableActionInterface scriptAction) {
        if (getRecorderStatus() == RECORDING) {
            scriptAction.insertScriptLine();
            // no notification is necessary since the scriptAction must call the other addLine() method to actually add to the script
        }
    }

    /**
     * Appends a comment line to the current script (if one is being recorded).
     *
     * @param  line  A comment to be added to the script (should not include an end-line character).
     */
    public synchronized void addCommentLine(String line) {
        if (getRecorderStatus() == RECORDING) {
            script += "# " + line.trim() + LINE_SEPARATOR;
            
            Preferences.debug("script recorder:\tLine added:\t# " + line.trim() + "\n", Preferences.DEBUG_SCRIPTING);
            
            notifyListenersOfScriptChange();
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
            script += action + "(" + parameterList.convertToString() + ")" + LINE_SEPARATOR;
            
            Preferences.debug("script recorder:\tLine added:\t" + action + "(" + parameterList.convertToString() + ")" + "\n", Preferences.DEBUG_SCRIPTING);
            
            notifyListenersOfScriptChange();
        }
    }

    /**
     * Erases the current script.
     */
    public synchronized void resetScript() {
        script = new String();
        imageTable = new ImageVariableTable();
        
        Preferences.debug("script recorder:\tScript reset" + "\n", Preferences.DEBUG_SCRIPTING);
        
        notifyListenersOfScriptChange();
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
     * Returns the current script.
     *
     * @return  The script being recorded.
     */
    public synchronized String getScript() {
        return script;
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
        Preferences.debug("script recorder:\tStatus changed from\t" + statusStrings[recorderStatus] + "\tto\t" + statusStrings[status] + "\n", Preferences.DEBUG_SCRIPTING);
        
        recorderStatus = status;
        
        notifyListenersOfScriptStatus();
    }
    
    /**
     * Changes the script being recorded by this class.  Used by the script recorder dialog to read in a script from disk or allow the user to manually edit the script.
     * @param  newScript  The new script text.
     */
    public synchronized void setScript(String newScript) {
        script = newScript;
        
        Preferences.debug("script recorder:\tScript explictly set from outside of the recorder." + "\n", Preferences.DEBUG_SCRIPTING);
        
        notifyListenersOfScriptChange();
    }
    
    /**
     * Starts the recording of a new script (if one isn't already being recorded).  Any script text which was 
     * previously recorded and stored in the recorder is erased (unless we are resuming from a recorder pause).
     * 
     * @return  <code>True</code> if recording was started successfully, <code>false</code> otherwise.
     */
    public synchronized boolean startRecording() {
        if (getRecorderStatus() == STOPPED) {
            Preferences.debug("script recorder:\tNew script started." + "\n", Preferences.DEBUG_SCRIPTING);
            resetScript();
            setRecorderStatus(RECORDING);
            return true;
        } else if (getRecorderStatus() == PAUSED) {
            Preferences.debug("script recorder:\tresumed." + "\n", Preferences.DEBUG_SCRIPTING);
            setRecorderStatus(RECORDING);
            return true;
        } else {
            MipavUtil.displayError("Script recording startup failed.  Cannot record two scripts at the same time.");
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
        Preferences.debug("script recorder:\tStoring image:\t" + imageName + "\n", Preferences.DEBUG_SCRIPTING);
        return getImageTable().storeImageName(imageName);
    }
    
    /**
     * Adds a class to the recorder's list of listeners.
     * @param  listener  A class which wants to be notified about changes in the recorder's status and script text.
     */
    public synchronized void addScriptRecordingListener(ScriptRecordingListener listener) {
        Preferences.debug("script recorder:\tNew listener added:\t" + listener.getClass().getName() + "\n", Preferences.DEBUG_SCRIPTING);
        recordingListeners.add(listener);
    }
    
    /**
     * Removes a listener from the recorder.
     * @param  listener  A class which no longer wants to know about changes in the recorder's state.
     */
    public synchronized void removeScriptRecordingListener(ScriptRecordingListener listener) {
        Preferences.debug("script recorder:\tListener removed:\t" + listener.getClass().getName() + "\n", Preferences.DEBUG_SCRIPTING);
        recordingListeners.remove(listener);
    }
    
    /**
     * Notifies any ScriptRecorderListeners of the current script text, after it has changed in some manner.
     */
    protected synchronized void notifyListenersOfScriptChange() {
        Preferences.debug("script recorder:\tNotifying listeners of a script change." + "\n", Preferences.DEBUG_SCRIPTING);
        for (int i = 0; i < recordingListeners.size(); i++) {
            ((ScriptRecordingListener)recordingListeners.elementAt(i)).updateScript(getScript());
        }
    }
    
    /**
     * Notifies any ScriptRecorderListeners of the current recording status of the script recorder.
     */
    protected synchronized void notifyListenersOfScriptStatus() {
        Preferences.debug("script recorder:\tNotifying listeners of a recorder status change." + "\n", Preferences.DEBUG_SCRIPTING);
        for (int i = 0; i < recordingListeners.size(); i++) {
            ((ScriptRecordingListener)recordingListeners.elementAt(i)).changeRecordingStatus(getRecorderStatus());
        }
    }
}
