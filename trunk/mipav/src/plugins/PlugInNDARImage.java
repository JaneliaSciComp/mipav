import gov.nih.mipav.plugins.PlugInGeneric;

import gov.nih.mipav.view.ViewUserInterface;


public class PlugInNDARImage implements PlugInGeneric {
    public void run() {
        new PlugInDialogNDAR(ViewUserInterface.getReference().getMainFrame());
    }
}
