import gov.nih.mipav.plugins.PlugInGeneric;


/**
 * Main entry point to NINDS Anonymization Plugin
 * 
 * @author senseneyj
 * 
 */
public class PlugInNINDSIdentificationTool implements PlugInGeneric {

    /**
     * constructor
     */
    public PlugInNINDSIdentificationTool() {

    }

    /**
     * run
     */
    public void run() {
        new PlugInDialogNINDSIdentificationTool(false);
    }

}
