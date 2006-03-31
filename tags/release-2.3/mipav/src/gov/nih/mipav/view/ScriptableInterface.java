package gov.nih.mipav.view;

import gov.nih.mipav.model.algorithms.AlgorithmScriptParser;
import gov.nih.mipav.model.algorithms.AlgorithmBase;

/**
 * An interface for classes which want to allow themselves to be scripted.
 * It should be implemented by any class which wants to be able to be called
 * from the script parser in <code>AlgorithmScriptParser</code>.
 * <br>
 * <br>
 * To make an operation scriptable:
 * <ul>
 * <li>have a class implement this interface (see JDialogGaussianBlur for an example implementation)</li>
 * <li>make sure the class is named JDialog* and its script command (generated in <code>insertScriptLine</code>)</li>
 * <li>include a default constructor (which doesn't have to do anything, but must exist)</li>
 *
 * @see JDialogGaussianBlur
 * @see AlgorithmScriptParser
 *
 * @author Evan McCreedy
 * @version 1.0 June 23, 2004
 */
public interface ScriptableInterface {
    /**
     * Sets up the dialog state and then starts it.
     * @param parser the script parser
     * @throws IllegalArgumentException if there is a problem with the algorithm arguments
     */
    public void scriptRun(AlgorithmScriptParser parser) throws IllegalArgumentException;

    /**
     * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
     * @param algo the algorithm to make an entry for
     */
    public void insertScriptLine(AlgorithmBase algo);
}
