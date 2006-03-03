package gov.nih.mipav.plugins;

import gov.nih.mipav.view.*;


public interface PlugInFile extends PlugIn {

    /**
    *   run
    *   @param UI           MIPAV main user interface.
    */
    public void run(ViewUserInterface UI);
}
