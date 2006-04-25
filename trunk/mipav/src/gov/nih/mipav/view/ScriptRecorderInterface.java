package gov.nih.mipav.view;


/**
 * Interface for classes able to record script actions.
 *
 * @author   Evan McCreedy
 * @version  1.0 July 13, 2004
 */
public interface ScriptRecorderInterface {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Appends the script text area with the message. Used by other classes to record commands from the GUI (such as
     * <code>OpenImage</code>).
     *
     * @param  appMessage  The command or information to record
     */
    void append(String appMessage);

    /**
     * Changes the key associated with an active image variable, solely for use with a "ChangeName" call in a script
     * (changing the image name).
     *
     * @param  oldKey  String old image key
     * @param  newKey  String new image key
     */
    void changeActiveVar(String oldKey, String newKey);

    /**
     * Changes the key associated with an image variable, solely for use with a "ChangeName" call in a script (changing
     * the image name).
     *
     * @param  oldKey  String old image key
     * @param  newKey  String new image key
     */
    void changeVar(String oldKey, String newKey);

    /**
     * Get active image flag (whether the script is working on the active image or a named image)..
     *
     * @return  <code>true</code> if , <code>false</code> if not
     */
    boolean getActiveImageFlag();

    /**
     * Get image number from the active image table.
     *
     * @param   key  active image name
     *
     * @return  active image number corresponding to the given image name
     */
    Object getActiveImgTableVar(String key);

    /**
     * Get image number from the image table.
     *
     * @param   key  image name
     *
     * @return  image number corresponding to the given image name
     */
    Object getImgTableVar(String key);

    /**
     * Returns the variable name associated with an image name. For example, if the user opens an image "test.img", the
     * name "test.img" is stored as a key to the variable name $image1. From then on, whenever the user refers to the
     * image "test.img", the variable name $image1 is recorded instead. This is so that when the user runs the script on
     * multiple images, the action takes place on a variable instead of an absolute (like "test.img").
     *
     * @param   key  Image name (if null, $active is used)
     *
     * @return  The variable name, such as $image1
     */
    Object getVar(String key);

    /**
     * Returns the variable name associated with a voi name. Like getVar, only for voi's.
     *
     * @param   key  the voi name
     *
     * @return  The variable name, such as $voi1
     */
    Object getVoiVar(String key);

    /**
     * Determines if the dialog is currently recording GUI actions.
     *
     * @return  <code>true</code> if currently recording, <code>false</code> if not
     */
    boolean isRecording();

    /**
     * Register the active image name in the active image table.
     *
     * @param  key  active image name
     */
    void putActiveVar(String key);

    /**
     * Stores an image name as a key to an image variable. The image variable is just $image plus a number (e.g.,
     * $image1, $image2, etc.). This must be called before <code>getVar</code> or else <code>getVar</code> will return a
     * null. Any time a new image is created in the GUI, this method ought to be called.
     *
     * @param  key  The image name to be associated with a new variable.
     */
    void putVar(String key);

    /**
     * Stores a voi name as a key to a voi variable. Like putVar, only for voi's.
     *
     * @param  key  The voi name to be associated with a new variable.
     */
    void putVoiVar(String key);

    /**
     * Removes the last appended line from the script.
     */
    void removeLine();

    /**
     * Whether the script is working on the active image or a named image. Set active image flag to true when script
     * start recording.
     *
     * @param  flag  the active image flag
     */
    void setActiveImageFlag(boolean flag);

    /**
     * Set the script recording flag.
     *
     * @param  flag  <code>true</code> if currently recording, <code>false</code> if not
     */
    void setRecording(boolean flag);
}
