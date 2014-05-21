package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.util.Vector;


/**
 * All scriptable dialogs should inherit from this abstract class. It contains helper methods which make script
 * running/recording easier.
 */
public abstract class JDialogScriptableBase extends JDialogBase implements ScriptableActionInterface {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1714695069613762132L;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Contains parameters used to run or record the dialog action, along with some common helper methods. */
    protected AlgorithmParameters scriptParameters = null;

    protected boolean displayInNewFrame;

    /**
     * Indicates whether the scripted algorithm completed successfully. Used to retain the status after the dialog has
     * finalized the algorithm handle(s) in algorithmPerformed.
     */
    protected boolean isComplete = false;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Passthrough to JDialogBase constructor.
     * 
     * @see JDialogBase
     */
    public JDialogScriptableBase() {
        super();
    }

    /**
     * Passthrough to JDialogBase constructor.
     * 
     * @param modal Whether the dialog is modal.
     * 
     * @see JDialogBase
     */
    public JDialogScriptableBase(final boolean modal) {
        super(modal);
    }

    /**
     * Passthrough to JDialogBase constructor.
     * 
     * @param parent The parent frame.
     * @param modal Whether the dialog is modal.
     * 
     * @see JDialogBase
     */
    public JDialogScriptableBase(final Frame parent, final boolean modal) {
        super(parent, modal);
    }

    /**
     * Passthrough to JDialogBase constructor.
     * 
     * @param parent The parent dialog.
     * @param modal Whether this dialog is modal.
     * 
     * @see JDialogBase
     */
    public JDialogScriptableBase(final Dialog parent, final boolean modal) {
        super(parent, modal);
    }
    
    /**
     * Passthrough to JDialogBase constructor.
     * 
     * @param parent The parent dialog.
     * @param modal Whether this dialog is modal.
     * @param menuVisible whether the preference loading menu should be displayed
     * 
     * @see JDialogBase
     */
    public JDialogScriptableBase(final Dialog parent, final boolean modal, final boolean menuVisible) {
        super(parent, modal, menuVisible);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Extracts the scripting action string which should be used for a given class.
     * 
     * @param dialogClass The class to get the script action string for (should be prefixed with JDialog).
     * 
     * @return The script action string (e.g., 'GaussianBlur' for 'gov.nih.mipav.view.dialogs.JDialogGaussianBlur').
     */
    public static final String getDialogActionString(final Class<? extends JDialogScriptableBase> dialogClass) {
        final String name = dialogClass.getName();
        String actionString = null;
        
        final Vector<String> scriptLocations = ScriptableActionLoader.getScriptActionLocations();
        for (String loc : scriptLocations) {
        	if (name.startsWith(loc)) {
        		actionString = name.substring(loc.length());
        		break;
        	}
        }

        if (actionString == null) {
        	// Display's the long package name, not a problem unless this script uses MIPAV tools.
            Preferences.debug("dialog base: No script prefix found.  Returning full class name " + name + "\n",
                    Preferences.DEBUG_SCRIPTING);

            return name;
        } else {
        	Preferences.debug("dialog base: Extracting script action command.  Returning " + actionString + "\n", Preferences.DEBUG_SCRIPTING);
        	return actionString;
        }
    }

    /**
     * If a script is being recorded and the action (read: algorithm) is done, add an entry for this action.
     */
    public void insertScriptLine() {

        // if either the scriptrecorder or provenancerecorder is running, add entries
        // must be done doubly as provenance and script image registers are completely different

        if (ScriptRecorder.getReference().getRecorderStatus() == ScriptRecorder.RECORDING) {
            final String action = JDialogScriptableBase.getDialogActionString(getClass());

            try {

                if (scriptParameters == null) {
                    scriptParameters = new AlgorithmParameters();
                }

                storeParamsFromGUI();
                ScriptRecorder.getReference().addLine(action, scriptParameters.getParams());
            } catch (final ParserException pe) {
                MipavUtil.displayError("Error encountered recording " + action + " scriptline:\n" + pe);
                pe.printStackTrace();

                String message = "script recorder:\tScript error:\t" + pe.getClass().getName() + "\n";

                for (int i = 0; i < pe.getStackTrace().length; i++) {
                    message += "\t" + pe.getStackTrace()[i] + "\n";
                }

                Preferences.debug(message, Preferences.DEBUG_SCRIPTING);
            }
        }

        if (ProvenanceRecorder.getReference().getRecorderStatus() == ProvenanceRecorder.RECORDING) {
            final String action = JDialogScriptableBase.getDialogActionString(getClass());

            try {
                // must new AlgorithmParameters regardless to tell it is for provenance
                scriptParameters = new DataProvenanceParameters();

                storeParamsFromGUI();
                ProvenanceRecorder.getReference().addLine(action, scriptParameters.getParams());
            } catch (final ParserException pe) {
                MipavUtil.displayError("Error encountered recording " + action + " scriptline:\n" + pe);
                pe.printStackTrace();

                String message = "script recorder:\tScript error:\t" + pe.getClass().getName() + "\n";

                for (int i = 0; i < pe.getStackTrace().length; i++) {
                    message += "\t" + pe.getStackTrace()[i] + "\n";
                }

                Preferences.debug(message, Preferences.DEBUG_SCRIPTING);
            }
        }

    }

    /**
     * Sets up the action dialog state and then executes it.
     * 
     * @param parameters Table of parameters for the script to use.
     * 
     * @throws IllegalArgumentException If there is a problem with the action arguments.
     */
    public void scriptRun(final ParameterTable parameters) throws IllegalArgumentException {
        scriptParameters = new AlgorithmParameters(parameters);

        setScriptRunning(true);
        setSeparateThread(false);

        setGUIFromParams();
        callAlgorithm();
        doPostAlgorithmActions();
    }

    /**
     * Starts the algorithm. Already exists in most algorithm dialogs. Should be called during scripted execution and
     * regular operation.
     */
    protected abstract void callAlgorithm();

    /**
     * Set the dialog GUI using the script parameters while running this algorithm as part of a script.
     */
    protected abstract void setGUIFromParams();

    /**
     * Record the parameters just used to run this algorithm in a script.
     * 
     * @throws ParserException If there is a problem creating/recording the new parameters.
     */
    protected abstract void storeParamsFromGUI() throws ParserException;

    /**
     * Used to perform actions after the execution of the algorithm is completed (e.g., put the result image in the
     * image table). Defaults to no action, override to actually have it do something.
     */
    protected void doPostAlgorithmActions() {}

    /**
     * Sets the flag to indicate whether the algorithm completed successfully. Used to retain the status after the
     * dialog has finalized the algorithm handle(s) in algorithmPerformed.
     * 
     * @param success True if the algorithm has finished successfully, false if it is not done yet or there was a
     *            problem.
     */
    protected void setComplete(final boolean success) {
        isComplete = success;
    }

    /**
     * Returns whether the algorithm completed successfully. Used to retain the status after the dialog has finalized
     * the algorithm handle(s) in algorithmPerformed.
     * 
     * @return True if the algorithm has finished successfully, false if it is not done yet or there was a problem.
     */
    protected boolean isComplete() {
        return isComplete;
    }
}
