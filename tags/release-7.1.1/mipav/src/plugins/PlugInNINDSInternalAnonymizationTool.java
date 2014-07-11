import gov.nih.mipav.plugins.PlugInGeneric;


/**
 * Main entry point to NINDS Internal Anonymization Plugin
 * 
 * @author joshim2
 * 
 */
public class PlugInNINDSInternalAnonymizationTool implements PlugInGeneric {

    /**
     * constructor
     */
    public PlugInNINDSInternalAnonymizationTool() {

    }

    /**
     * run
     */
    public void run() {
        new PlugInDialogNINDSInternalAnonymizationTool(false);
    }

}
