import gov.nih.mipav.plugins.PlugInGeneric;


/**
 * Main entry point to NINDS Anonymization Plugin
 * 
 * @author pandyan
 * 
 */
public class PlugInNINDSAnonymizationTool implements PlugInGeneric {

    /**
     * constructor
     */
    public PlugInNINDSAnonymizationTool() {

    }

    /**
     * run
     */
    public void run() {
        new PlugInDialogNINDSAnonymizationTool(false);
    }

}
