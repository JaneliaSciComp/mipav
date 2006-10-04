package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;


/**
 * Forces the JVM to perform a garbage collection of un-referenced memory.
 */
public class ActionCollectGarbage extends ActionBase {

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
        System.gc();
    }
}
