package gov.nih.mipav.model.scripting;


import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.*;

import java.util.Vector;


/**
 * Executes a script with a set of images.
 */
public class ScriptRunner {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** The reference to the only occurrance of this class. */
    protected static ScriptRunner singletonReference = null;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** The table containing image-placeholder-to-image-name mappings for the script we want to run. */
    protected ImageVariableTable imageTable;

    /** Indicates whether a script is currently being run by mipav. */
    protected boolean isRunning;

    /** The path to the script we want to execute. */
    protected String scriptFile;

    /** The table containing VOIs to be used in the script. */
    protected VOITable voiTable;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ScriptRunner object.
     */
    protected ScriptRunner() {
        isRunning = false;
        Preferences.debug("script runner:\tCreated." + "\n", Preferences.DEBUG_SCRIPTING);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Returns a reference to the script recorder.
     * 
     * @return A reference to the script recorder.
     */
    public static final synchronized ScriptRunner getReference() {

        if (ScriptRunner.singletonReference == null) {
            ScriptRunner.singletonReference = new ScriptRunner();
        }

        return ScriptRunner.singletonReference;
    }

    /**
     * Retrieves an image from the image table being used by the current script we are running.
     * 
     * @param imageVar The image placeholder variable associated with the image we want.
     * 
     * @return The image associated with the given image variable.
     */
    public synchronized ModelImage getImage(final String imageVar) {
        Preferences.debug("script runner:\tRetrieving image:\t" + imageVar + "\n", Preferences.DEBUG_SCRIPTING);

        return getImageTable().getImage(imageVar);
    }

    /**
     * Returns a reference to the image table being used to run the current script.
     * 
     * @return The image table which should be used to run the current script, or <code>null</code> if no script is
     *         being run at the moment.
     */
    public synchronized ImageVariableTable getImageTable() {

        if (isRunning()) {
            return imageTable;
        }

        MipavUtil.displayError("Scripting error: tried to retrieve the image table while no script is being run.");
        Preferences.debug("script runner:\tRetrieved image table while no script is being run." + "\n",
                Preferences.DEBUG_SCRIPTING);

        return null;
    }

    /**
     * Returns a reference to the VOI table being used to run the current script.
     * 
     * @return The VOI table which should be used to run the current script, or <code>null</code> if no script is
     *         being run at the moment.
     */
    public synchronized VOITable getVOITable() {

        if (isRunning()) {
            return voiTable;
        }

        MipavUtil.displayError("Scripting error: tried to retrieve the VOI table while no script is being run.");
        Preferences.debug("script runner:\tRetrieved VOI table while no script is being run." + "\n",
                Preferences.DEBUG_SCRIPTING);

        return null;
    }

    /**
     * Returns whether we are currently running a script.
     * 
     * @return <code>True</code> if a script is being run, <code>false</code> otherwise.
     */
    public synchronized boolean isRunning() {
        return isRunning;
    }

    /**
     * Execute a script, using a set of images.
     * 
     * @param file The path to the script file we want to run.
     * @param imageNameList The list of the names of images to use while executing the script.
     * @param voiPathList A list of VOI paths, in the order returned by Parser.getImageVarsUsedInScript().
     * 
     * @return <code>True</code> if execution of the script was successful, <code>false</code> otherwise.
     */
    public synchronized boolean runScript(final String file, final Vector<String> imageNameList,
            final Vector<String> voiPathList) {

        if (isRunning()) {
            MipavUtil.displayError("A script is already being executed.");

            return false;
        }

        Preferences.debug("script runner:\tStarting script execution:\t" + file + "\n", Preferences.DEBUG_SCRIPTING);

        setRunning(true);

        scriptFile = file;
        imageTable = new ImageVariableTable();

        // ScriptThread thread;

        try {
            fillImageTable(imageNameList);

            voiTable = new VOITable(scriptFile, voiPathList);

            // thread = new ScriptThread(scriptFile);
            // thread.start();

            Parser.runScript(scriptFile);

            Preferences.debug("script runner:\tFinished script execution:\t" + scriptFile + "\n",
                    Preferences.DEBUG_SCRIPTING);

            setRunning(false);
        } catch (final ParserException pe) {
            handleParserException(pe);

            return false;
        }

        return true;
    }

    /**
     * Convenience method used to store the name of an image in the image table being used by the current script.
     * 
     * @param imageName The name of the image to store.
     * 
     * @return The image variable placeholder which has been assigned to the image name (may not be a new variable if
     *         the name is already in the table).
     */
    public synchronized String storeImage(final String imageName) {
        Preferences.debug("script runner:\tStoring image:\t" + imageName + "\n", Preferences.DEBUG_SCRIPTING);

        return getImageTable().storeImageName(imageName);
    }

    /**
     * Populate the image table based on a list of image names we want to use in the execution of the script.
     * 
     * @param imageNameList A list of image names, in the order they should be used in the script.
     * 
     * @throws ParserException If there is a problem encountered while reading the image variables used in the script.
     */
    protected synchronized void fillImageTable(final Vector<String> imageNameList) throws ParserException {
        final String[] imageVars = Parser.getImageVarsUsedInScript(scriptFile);

        for (int i = 0; i < imageNameList.size(); i++) {
            imageTable.put(imageVars[i], imageNameList.elementAt(i));

            Preferences.debug("script runner:\tAdded image to image table:\t" + imageVars[i] + "\t->\t"
                    + imageNameList.elementAt(i) + "\n", Preferences.DEBUG_SCRIPTING);
        }
    }

    /**
     * Handle a script parser exception by printing out the location of the problem and stopping script execution.
     * 
     * @param pe The parser exception to handle.
     */
    protected void handleParserException(final ParserException pe) {
        Preferences.debug("script runner:\tAborted script execution:\t" + pe.getParsedFileName() + "\n",
                Preferences.DEBUG_SCRIPTING);

        // if this exception was caused by another exception, print it to stderr
        if (pe.getCause() != null) {
            pe.printStackTrace();

            String message = "script runner:\tScript error:\t" + pe.getCause().getClass().getName() + "\n";

            for (int i = 0; i < pe.getCause().getStackTrace().length; i++) {
                message += "\t" + pe.getCause().getStackTrace()[i] + "\n";
            }

            Preferences.debug(message, Preferences.DEBUG_SCRIPTING);
        }

        MipavUtil.displayError("Error executing script:\n" + pe);

        setRunning(false);
    }

    /**
     * Changes the flag indicating whether a script is currently being run.
     * 
     * @param running Whether we are running a script.
     */
    public synchronized void setRunning(final boolean running) {
        isRunning = running;
    }

    /**
     * Changes the image variable table for the current script execution.
     * 
     * @param table A new image variable table.
     */
    public synchronized void setImageTable(final ImageVariableTable table) {
        imageTable = table;
    }

    // ~ Inner Classes
    // --------------------------------------------------------------------------------------------------

    /**
     * A separate thread used to execute a given script (the image and voi table should already be set up before the
     * thread is started).
     */
    private class ScriptThread extends Thread {

        /** The file name of the script to execute in this thread. */
        private final String curScriptFile;

        /**
         * Creates a new ScriptThread object.
         * 
         * @param file The script file to execute in this thread.
         */
        public ScriptThread(final String file) {
            curScriptFile = file;
        }

        /**
         * Start execution of the script.
         */
        public void run() {

            try {
                Parser.runScript(curScriptFile);

                setRunning(false);

                Preferences.debug("script runner:\tFinished script execution:\t" + curScriptFile + "\n",
                        Preferences.DEBUG_SCRIPTING);
            } catch (final ParserException pe) {
                handleParserException(pe);
            }

            synchronized (ScriptRunner.getReference()) {
                ScriptRunner.getReference().notifyAll();
            }
        }
    }
}
