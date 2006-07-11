package gov.nih.mipav.model.scripting;


import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.*;
import java.util.*;

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
 * @author   Nathan Pollack
 * @version  1.0 June 28, 2006
 */
public class ScriptParameters extends AlgorithmParameters {
 
    //~ Methods --------------------------------------------------------------------------------------------------------

        
        
        public void doPostAlgorithmActions() {}
        public void storeParamsFromGUI() throws ParserException {}
        public void setGUIFromParams() {}
}