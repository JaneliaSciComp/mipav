package gov.nih.mipav.model.scripting;


import gov.nih.mipav.model.scripting.parameters.ParameterTable;

import gov.nih.mipav.view.ViewUserInterface;


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

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ScriptRecorder object.  May only be called by <code>getReference()</code> since this is a singleton class.
     */
    protected ScriptRecorder() {
        script = new String();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Returns a reference to the script recorder.
     *
     * @return  A reference to the script recorder.
     */
    public static final ScriptRecorder getReference() {

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
    public void addLine(String line) {

        if (isRecording()) {
            script += line.trim() + LINE_SEPARATOR;
        }
    }

    /**
     * Appends a line of text to the current script (if one is being recorded) using a script action and list of parameters.
     *
     * @param  action         The action to put into the new script line.
     * @param  parameterList  The list of parameters to include in the new script line.
     */
    public void addLine(String action, ParameterTable parameterList) {

        if (isRecording()) {
            script += action + "(" + parameterList + ")" + LINE_SEPARATOR;
        }
    }

    /**
     * Erases the current script.
     */
    public void clearScript() {
        script = new String();
    }

    /**
     * Returns the current script.
     *
     * @return  The script being recorded.
     */
    public String getScript() {
        return script;
    }

    /**
     * Checks to see if a script is being recorded right now.
     *
     * @return  <code>True</code> if a script is being recorded right now, <code>false</code> otherwise.
     */
    public boolean isRecording() {
        return ViewUserInterface.getReference().isScriptRecording();
    }
}
