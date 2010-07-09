package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewUserInterface;


/**
 * A script action which records saves the text contained in one of the output frame's tabs.
 */
public class ActionSaveTab implements ScriptableActionInterface {

    /**
     * The label to use for the tab name parameter.
     */
    private static final String TAB_NAME_LABEL = "output_tab_name";
    
    /**
     * The name of the tab to record the saving of.
     */
    private String recordingTabName;
    
    /**
     * Constructor for the dynamic instantiation and execution of the SaveTab script action.
     */
    public ActionSaveTab() {}
    
    /**
     * Constructor used to record the SaveTab script action line.
     * @param tabName  The name of the tab being saved.
     */
    public ActionSaveTab(String tabName) {
        recordingTabName = tabName;
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
    public void insertScriptLine() {
        ParameterTable parameters = new ParameterTable();
        try {
            parameters.put(ParameterFactory.newString(TAB_NAME_LABEL, recordingTabName));
        } catch (ParserException pe) {
            MipavUtil.displayError("Error encountered creating tab name parameter while recording SaveTab script action:\n" + pe);
            return;
        }
        
        ScriptRecorder.getReference().addLine("SaveTab", parameters);
    }

    /**
     * {@inheritDoc}
     */
    public void scriptRun(ParameterTable parameters) {
        ViewUserInterface.getReference().getMessageFrame().save(parameters.getString(TAB_NAME_LABEL));
    }
    
    /**
     * Changes the name of the tab being saved.
     * @param tabName  The name of the tab being saved.
     */
    public void setTabName(String tabName) {
        recordingTabName = tabName;
    }
}
