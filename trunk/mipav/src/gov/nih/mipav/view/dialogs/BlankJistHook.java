package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.scripting.parameters.ParameterTable;

/**
 * This class is an example class for tying existing plugins into the JIST interface.  This class can be
 * inserted into any build of MIPAV which has a compatible build of JIST.  JIST will then discover the plugin
 * and execute it appropriately.
 * 
 * @author senseneyj
 *
 */
public class BlankJistHook {//implements ActionDiscovery {

    private JDialogSWI internalDialog;
    
    public BlankJistHook() {
        //this.internalDialog = new JDialogSWI();
    }
    
    public ActionMetadata getActionMetadata() {
        return internalDialog.getActionMetadata();
    }

    public ParameterTable createInputParameters() {
        return internalDialog.createInputParameters();
    }

    public ParameterTable createOutputParameters() {
        return internalDialog.createOutputParameters();
    }

    public void scriptRun(ParameterTable table) {
        internalDialog.scriptRun(table);
    }

    public String getOutputImageName(String imageParamName) {
        return internalDialog.getOutputImageName(imageParamName);
    }

    public boolean isActionComplete() {
        return internalDialog.isActionComplete();
    }

}
