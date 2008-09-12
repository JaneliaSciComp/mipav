package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;


/**
 * A script action which causes MIPAV to exit completely.
 */
public class ActionExit extends ActionBase {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
    public void insertScriptLine() {
        ScriptRecorder.getReference().addLine(getActionName(), new ParameterTable());
    }

    /**
     * {@inheritDoc}
     */
    public void scriptRun(ParameterTable parameters) {
        System.exit(0);
    }
}
