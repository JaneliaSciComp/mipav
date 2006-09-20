package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;

import gov.nih.mipav.view.*;

import java.awt.*;


/**
 * All scriptable dialogs should inherit from this abstract class. It contains helper methods which make script
 * running/recording easier.
 */
public abstract class JDialogScriptableBase extends JDialogBase implements ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1714695069613762132L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Contains parameters used to run or record the dialog action, along with some common helper methods. */
    protected AlgorithmParameters scriptParameters = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Passthrough to JDialogBase constructor.
     *
     * @see  JDialogBase
     */
    public JDialogScriptableBase() {
        super();
    }

    /**
     * Passthrough to JDialogBase constructor.
     *
     * @param  modal  Whether the dialog is modal.
     *
     * @see    JDialogBase
     */
    public JDialogScriptableBase(boolean modal) {
        super(modal);
    }

    /**
     * Passthrough to JDialogBase constructor.
     *
     * @param  parent  The parent frame.
     * @param  modal   Whether the dialog is modal.
     *
     * @see    JDialogBase
     */
    public JDialogScriptableBase(Frame parent, boolean modal) {
        super(parent, modal);
    }

    /**
     * Passthrough to JDialogBase constructor.
     *
     * @param  parent  The parent dialog.
     * @param  modal   Whether this dialog is modal.
     *
     * @see    JDialogBase
     */
    public JDialogScriptableBase(Dialog parent, boolean modal) {
        super(parent, modal);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Extracts the scripting action string which should be used for a given class.
     *
     * @param   dialogClass  The class to get the script action string for (should be prefixed with JDialog).
     *
     * @return  The script action string (e.g., 'GaussianBlur' for 'gov.nih.mipav.view.dialogs.JDialogGaussianBlur').
     */
    public static final String getDialogActionString(Class dialogClass) {
        String classPrefix = "JDialog";
        String name = dialogClass.getName();
        int index = name.lastIndexOf(classPrefix);

        if (index == -1) {

            // TODO: may be an fatal error..
            Preferences.debug("dialog base: No script " + classPrefix + " prefix found.  Returning " + name + "\n",
                              Preferences.DEBUG_SCRIPTING);

            return name;
        } else {
            Preferences.debug("dialog base: Extracting script action command.  Returning " +
                              name.substring(index + classPrefix.length()) + "\n", Preferences.DEBUG_SCRIPTING);

            return name.substring(index + classPrefix.length());
        }
    }

    /**
     * If a script is being recorded and the action (read: algorithm) is done, add an entry for this action.
     */
    public void insertScriptLine() {

        if (ScriptRecorder.getReference().getRecorderStatus() == ScriptRecorder.RECORDING) {
            String action = getDialogActionString(getClass());

            try {

                if (scriptParameters == null) {
                    scriptParameters = new AlgorithmParameters();
                }

                storeParamsFromGUI();
                ScriptRecorder.getReference().addLine(action, scriptParameters.getParams());
            } catch (ParserException pe) {
                MipavUtil.displayError("Error encountered recording " + action + " scriptline:\n" + pe);
                pe.printStackTrace();
            }
        }
    }

    /**
     * Sets up the action dialog state and then executes it.
     *
     * @param   parameters  Table of parameters for the script to use.
     *
     * @throws  IllegalArgumentException  If there is a problem with the action arguments.
     */
    public void scriptRun(ParameterTable parameters) throws IllegalArgumentException {
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
     * @throws  ParserException  If there is a problem creating/recording the new parameters.
     */
    protected abstract void storeParamsFromGUI() throws ParserException;

    /**
     * Used to perform actions after the execution of the algorithm is completed (e.g., put the result image in the
     * image table). Defaults to no action, override to actually have it do something.
     */
    protected void doPostAlgorithmActions() { }
}
