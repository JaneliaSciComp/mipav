package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;


/**
 * Forces the JVM to perform a garbage collection of un-referenced memory.
 */
public class ActionCollectGarbage implements ScriptableActionInterface {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
    public void insertScriptLine() {
        ScriptRecorder.getReference().addLine("CollectGarbage");
    }

    /**
     * {@inheritDoc}
     */
    public void scriptRun(ParameterTable parameters) {
        System.gc();
    }
}
