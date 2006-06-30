package gov.nih.mipav.model.scripting;


import gov.nih.mipav.model.scripting.parameters.ParameterTable;

import gov.nih.mipav.view.MipavUtil;

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
        }
    }

    /**
     * Appends a line of text to the current script (if one is being recorded).
     *
     * @param  line  A line of text to be added to the script (should not include an end-line character).
     */
    public synchronized void addLine(String line) {
        if (getRecorderStatus() == RECORDING) {
            script += line.trim() + LINE_SEPARATOR;
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
            script += action + "(" + parameterList + ")" + LINE_SEPARATOR;
            
            notifyListenersOfScriptChange();
        }
    }

    /**
     * Erases the current script.
     */
    public synchronized void resetScript() {
        script = new String();
        imageTable = new ImageVariableTable();
        
        notifyListenersOfScriptChange();
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
        recorderStatus = status;
        
        notifyListenersOfScriptStatus();
    }
    
    /**
     * Changes the script being recorded by this class.  Used by the script recorder dialog to read in a script from disk or allow the user to manually edit the script.
     * @param  newScript  The new script text.
     */
    public synchronized void setScript(String newScript) {
        script = newScript;
        
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
            resetScript();
            setRecorderStatus(RECORDING);
            return true;
        } else if (getRecorderStatus() == PAUSED) {
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
            setRecorderStatus(STOPPED);
        } else {
            MipavUtil.displayError("Cannot stop script recording.  No recording is currently taking place.");
            return;
        }
    }
    
    /**
     * Adds a class to the recorder's list of listeners.
     * @param  listener  A class which wants to be notified about changes in the recorder's status and script text.
     */
    public synchronized void addScriptRecordingListener(ScriptRecordingListener listener) {
        recordingListeners.add(listener);
    }
    
    /**
     * Removes a listener from the recorder.
     * @param  listener  A class which no longer wants to know about changes in the recorder's state.
     */
    public synchronized void removeScriptRecordingListener(ScriptRecordingListener listener) {
        recordingListeners.remove(listener);
    }
    
    /**
     * Notifies any ScriptRecorderListeners of the current script text, after it has changed in some manner.
     */
    protected synchronized void notifyListenersOfScriptChange() {
        for (int i = 0; i < recordingListeners.size(); i++) {
            ((ScriptRecordingListener)recordingListeners.elementAt(i)).updateScript(getScript());
        }
    }
    
    /**
     * Notifies any ScriptRecorderListeners of the current recording status of the script recorder.
     */
    protected synchronized void notifyListenersOfScriptStatus() {
        for (int i = 0; i < recordingListeners.size(); i++) {
            ((ScriptRecordingListener)recordingListeners.elementAt(i)).changeRecordingStatus(getRecorderStatus());
        }
    }
}
