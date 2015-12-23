package gov.nih.mipav.view.dialogs;

import mtry.PlugInDialogMTry534d;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;

/**
 * This class is an example class for tying existing plugins into the JIST interface.  This class can be
 * inserted into any build of MIPAV which has a compatible build of JIST.  JIST will then discover the plugin
 * and execute it appropriately.
 * 
 * @author senseneyj
 *
 */
public class MTryJistHook implements ActionDiscovery {

    private PlugInDialogMTry534d internalDialog;
    
    public MTryJistHook() {
        this.internalDialog = new PlugInDialogMTry534d();
    }
    
    public ActionMetadata getActionMetadata() {
        return internalDialog.getActionMetadata();
    }

    @Override
    public ParameterTable createInputParameters() {
        return internalDialog.createInputParameters();
    }

    @Override
    public ParameterTable createOutputParameters() {
        return internalDialog.createOutputParameters();
    }

    @Override
    public void scriptRun(ParameterTable table) {
        internalDialog.scriptRun(table);
    }

    @Override
    public String getOutputImageName(String imageParamName) {
        return internalDialog.getOutputImageName(imageParamName);
    }

    @Override
    public boolean isActionComplete() {
        return internalDialog.isActionComplete();
    }

}
