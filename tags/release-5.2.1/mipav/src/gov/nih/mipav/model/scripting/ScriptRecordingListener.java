package gov.nih.mipav.model.scripting;

/**
 * An interface for classes which want to be notified of changes to the script currently being recorded by MIPAV.
 */
public interface ScriptRecordingListener {
    /**
     * Alerts the listener to a change in the text of the script currently being recorded.
     * @param  newScriptText  The text of the script (the whole text, not just any additions).
     */
    void updateScript(String newScriptText);
    
    /**
     * Alerts the listener to a change in whether the ScriptRecorder is currently recording a script.
     * @param  recorderStatus  The new status of the scriptRecorder (PAUSED, RECORDING, or STOPPED).
     */
    void changeRecordingStatus(int recorderStatus);
}
