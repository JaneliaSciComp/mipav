package gov.nih.mipav.view;


/**
 * <p>Title: Dialog Defaults Interface</p>
 *
 * <p>Description: Simple interface for loading and saving default settings for dialogs. load defaults should be called
 * from the constructor that includes a ViewJFrameImage (the only constructor that calls init()), because it is only
 * used for setting default settings within a visible dialog and is not meant for scripting.</p>
 *
 * <p>saveDefaults() should be called in the algorithmPerformed() method if Preferences.isSaveDialogDefaults()</p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Company:</p>
 *
 * @author   ben link
 * @version  1.0
 */

public interface DialogDefaultsInterface {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    void loadDefaults();

    /**
     * DOCUMENT ME!
     */
    void saveDefaults();

}
