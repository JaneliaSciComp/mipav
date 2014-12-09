package gov.nih.mipav.view;


/**
 * <p>Title: Dialog Defaults Interface</p>
 *
 * <p>Description: Simple interface for loading and saving default settings for dialogs. It is only
 * used for setting default settings within a visible dialog and is not meant for scripting.</p>
 *
 * <p>saveDefaults() should be called in the algorithmPerformed() method if Preferences.isSaveDialogDefaults()</p>
 *
 * <p>Copyright: Copyright (c) 2012</p>
 *
 * <p>Company:The National Institutes of Health</p>
 * @deprecated
 * @author   ben link
 * @author   Justin Senseney
 * @version  1.0
 */

public interface LegacyDialogDefaultsInterface {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Legacy method for loading dialog defaults, requires dialog to define parameters that should be saved.
     * 
     * @deprecated JDialogBase automatically finds a default profile using loadDefaults().
     */
    void legacyLoadDefaults();

    /**
     * Legacy method for saving dialog defaults, requires dialog to define parameters that should be loaded.
     * 
     * @deprecated JDialogBase automatically saves a default profile using savesDefaults().
     */
    void legacySaveDefaults();

}
