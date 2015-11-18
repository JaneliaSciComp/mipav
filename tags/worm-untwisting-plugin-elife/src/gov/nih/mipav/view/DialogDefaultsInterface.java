package gov.nih.mipav.view;


/**
 * <p>
 * Simple interface for loading and saving default settings for dialogs. Load defaults should be called
 * from the constructor that includes a ViewJFrameImage (the only constructor that calls init()), because it is only
 * used for setting default settings within a visible dialog and is not meant for scripting.
 * </p>
 * <p>
 * This interface is used by JDialogBase as part of the auto-default save/load functionality.  Also used by some
 * external plugins as part of the old method of saving/loading defaults manually for specific dialogs.
 * </p> 
 *
 * <p>saveDefaults() should be called in the algorithmPerformed() method if Preferences.isSaveDialogDefaults()</p>
 * 
 * @author   ben link
 */
public interface DialogDefaultsInterface {

    //~ Methods --------------------------------------------------------------------------------------------------------

	/**
     * Method for loading dialog defaults.
     */
    void loadDefaults();

    /**
     * Method for loading dialog defaults.
     */
    void saveDefaults();

}
