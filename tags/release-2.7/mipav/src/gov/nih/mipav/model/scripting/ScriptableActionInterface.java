package gov.nih.mipav.model.scripting;


import gov.nih.mipav.model.scripting.parameters.ParameterTable;


/**
 * An interface for classes which want to allow themselves to be scripted. It should be implemented by any class which
 * wants to be able to be called from the script parser in <code>Parser</code>.<br>
 * <br>
 * To make an operation scriptable:
 *
 * <ul>
 *   <li>have a class implement this interface (see JDialogGaussianBlur for an example implementation)</li>
 *   <li>make sure the class is named JDialog* and its script command (generated in <code>insertScriptLine()</code>)</li>
 *   <li>include a default constructor (which doesn't have to do anything, but must exist)</li>
 * </ul>
 *
 * @see      gov.nih.mipav.view.dialogs.JDialogGaussianBlur
 * @see      gov.nih.mipav.view.dialogs.AlgorithmParameters
 * @author   Evan McCreedy
 * @version  1.0 June 23, 2004
 */
public interface ScriptableActionInterface {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * If a script is being recorded and the action (read: algorithm) is done, add an entry for this action.
     */
    void insertScriptLine();

    /**
     * Sets up the action dialog state and then executes it.
     *
     * @param   parameters  Table of parameters for the script to use.
     *
     * @throws  IllegalArgumentException  If there is a problem with the action arguments.
     */
    void scriptRun(ParameterTable parameters) throws IllegalArgumentException;
}
