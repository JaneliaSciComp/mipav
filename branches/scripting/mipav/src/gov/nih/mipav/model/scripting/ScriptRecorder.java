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

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The contents of the current script being recorded. */
    protected String script;
    
    /** Whether we are currently recording a script. */
    protected boolean isRecording;
    
    /** A list of listeners who want to know about lines added to the script. */
    protected Vector recordingListeners = new Vector();

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ScriptRecorder object.  May only be called by <code>getReference()</code> since this is a singleton class.
     */
    protected ScriptRecorder() {
        script = new String();
        isRecording = false;
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
     * Appends a line of text to the current script (if one is being recorded).
     *
     * @param  line  A line of text to be added to the script (should not include an end-line character).
     */
    public synchronized void addLine(String line) {

        if (isRecording()) {
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

        if (isRecording()) {
            script += action + "(" + parameterList + ")" + LINE_SEPARATOR;
            
            notifyListenersOfScriptChange();
        }
    }

    /**
     * Erases the current script.
     */
    public synchronized void clearScript() {
        script = new String();
        
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
     * Checks to see if a script is being recorded right now.
     *
     * @return  <code>True</code> if a script is being recorded right now, <code>false</code> otherwise.
     */
    public synchronized boolean isRecording() {
        return isRecording;
    }
    
    /**
     * Changes whether the script recorder is currently recording.  Also notifies any ScriptRecorderListeners of the change.
     * @param  recording  Whether we are currently recording a script.
     */
    protected synchronized void setRecording(boolean recording) {
        isRecording = recording;
        
        notifyListenersOfScriptStatus();
    }
    
    /**
     * Starts the recording of a new script (if one isn't already being recorded).  Any script text which was previously recorded and stored in the recorder is erased.
     * @return  <code>True</code> if recording was started successfully, <code>false</code> otherwise.
     */
    public synchronized boolean startRecording() {
        if (!isRecording()) {
            clearScript();
            setRecording(true);
            return true;
        } else {
            MipavUtil.displayError("Script recording startup failed.  Cannot record two scripts at the same time.");
            return false;
        }
    }
    
    /**
     * Pauses or stops script recording.  Call <code>getScript()</code> to get the recorded script.
     */
    public synchronized void stopRecording() {
        if (isRecording()) {
            setRecording(false);
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
            ((ScriptRecordingListener)recordingListeners.elementAt(i)).changeRecordingStatus(isRecording());
        }
    }
}
