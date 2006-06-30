package gov.nih.mipav.model.scripting;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

import gov.nih.mipav.model.structures.ModelImage;

import java.util.Vector;


/**
 * Executes a script with a set of images.
 */
public class ScriptRunner {
    //~ Static fields/initializers -------------------------------------------------------------------------------------
    
    /** The reference to the only occurrance of this class. */
    protected static ScriptRunner singletonReference = null;
    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Indicates whether a script is currently being run by mipav. */
    protected boolean isRunning;
    
    /** The path to the script we want to execute. */
    protected String scriptFile;
    
    /** The table containing image-placeholder-to-image-name mappings for the script we want to run. */
    protected ImageVariableTable imageTable;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ScriptRunner object.
     */
    protected ScriptRunner() {
        isRunning = false;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------
    
    /**
     * Returns a reference to the script recorder.
     *
     * @return  A reference to the script recorder.
     */
    public synchronized static final ScriptRunner getReference() {

        if (singletonReference == null) {
            singletonReference = new ScriptRunner();
        }

        return singletonReference;
    }
    
    /**
     * Returns whether we are currently running a script.
     * 
     * @return  <code>True</code> if a script is being run, <code>false</code> otherwise.
     */
    public synchronized boolean isRunning() {
        return isRunning;
    }
    
    /**
     * Execute a script, using a set of images.
     * 
     * @param   file           The path to the script file we want to run.
     * @param   imageNameList  The list of the names of images to use while executing the script.
     * 
     * @return  <code>True</code> if execution of the script was successful, <code>false</code> otherwise.
     */
    public synchronized boolean runScript(String file, Vector imageNameList) {
        if (isRunning()) {
            MipavUtil.displayError("A script is already being executed.");
            return false;
        }
        
        setRunning(true);
        
        scriptFile = new String();
        imageTable = new ImageVariableTable();
        
        try {
            String[] imageVarsNeeded = Parser.getImageVarsUsedInScript(scriptFile);
            if (imageVarsNeeded.length != imageNameList.size()) {
                MipavUtil.displayError("Not enough images provided while attempting to run the script.\n Found: " +
                        imageNameList.size() + " Required: " + imageVarsNeeded.length);
                setRunning(false);
                return false;
            }
            
            fillImageTable(imageNameList);
            
            Parser.runScript(scriptFile);
        } catch (ParserException pe) {
            MipavUtil.displayError("Error executing script:\n" + pe);
            setRunning(false);
            return false;
        }
        
        setRunning(false);
        return true;
    }
    
    /**
     * Populate the image table based on a list of image names we want to use in the execution of the script.
     * 
     * @param  imageNameList  A list of image names, in the order they should be used in the script.
     */
    protected synchronized void fillImageTable(Vector imageNameList) {
        for (int i = 0; i < imageNameList.size(); i++) {
            String imageName = (String)imageNameList.elementAt(i);
            String imageVar = imageTable.storeImageName(imageName);
            
            Preferences.debug("Added image to image table:\t" + imageVar + "\t->\t" + imageName, Preferences.DEBUG_MINOR);
        }
    }
    
    /**
     * Changes the flag indicating whether a script is currently being run.
     * 
     * @param  running  Whether we are running a script.
     */
    protected synchronized void setRunning(boolean running) {
        isRunning = running;
    }
    
    /**
     * Returns a reference to the image table being used to run the current script.
     * 
     * @return  The image table which should be used to run the current script, or <code>null</code> if no script is being run at the moment.
     */
    public synchronized ImageVariableTable getImageTable() {
        if (isRunning()) {
            return imageTable;
        }
        
        // TODO: should an exception be thrown or a message displayed?
        return null;
    }
    
    /**
     * Retrieves an image from the image table being used by the current script we are running.
     * 
     * @param   imageVar  The image placeholder variable associated with the image we want.
     * 
     * @return  The image associated with the given image variable.
     */
    public synchronized ModelImage getImage(String imageVar) {
        return getImageTable().getImage(imageVar);
    }
    
    /**
     * Convenience method used to store the name of an image in the image table being used by the current script.
     * 
     * @param   imageName  The name of the image to store.
     * 
     * @return  The image variable placeholder which has been assigned to the image name (may not be a new variable if the name is already in the table).
     */
    public synchronized String storeImage(String imageName) {
        return getImageTable().storeImageName(imageName);
    }
}
