package gov.nih.mipav.view;


/**
 * Interface for classes able to record script actions.
 *
 * @author Evan McCreedy
 * @version 1.0 July 13, 2004
 */
public interface ScriptRecorder {

    /**
     *  Appends the script text area with the message.  Used by other classes to record commands from the GUI (such as <code>OpenImage</code>).
     *
     *  @param appMessage  The command or information to record
     */
    public void append( String appMessage );

    /**
     *	Removes the last appended line from the script.
     */
    public void removeLine();

    /**
     *	Determines if the dialog is currently recording GUI actions.
     *
     *	@return <code>true</code> if currently recording, <code>false</code> if not
     */
    public boolean isRecording();

    /**
     * Set the script recording flag.
     * @param flag <code>true</code> if currently recording, <code>false</code> if not
     */
    public void setRecording( boolean flag );

    /**
     * Returns the variable name associated with an image name.  For example, if the user
     * opens an image "test.img", the name "test.img" is stored as a key to the variable name
     * $image1.  From then on, whenever the user refers to the image "test.img", the variable name
     * $image1 is recorded instead.  This is so that when the user runs the script on multiple images,
     * the action takes place on a variable instead of an absolute (like "test.img").
     *
     * @param key  Image name (if null, $active is used)
     * @return     The variable name, such as $image1
     */
    public Object getVar( String key );

    /**
     * Stores an image name as a key to an image variable.  The image variable is just $image
     * plus a number (e.g., $image1, $image2, etc.).  This must be called before <code>getVar</code>
     * or else <code>getVar</code> will return a null.  Any time a new image is created in the GUI, this
     * method ought to be called.
     * @param key  The image name to be associated with a new variable.
     */
    public void putVar( String key );

    /**
     * Changes the key associated with an image variable, solely for use with
     * a "ChangeName" call in a script (changing the image name)
     * @param oldKey String old image key
     * @param newKey String new image key
     */
    public void changeVar( String oldKey, String newKey);

    /**
     * Get image number from the image table.
     * @param key  image name
     * @return   image number corresponding to the given image name
     */
    public Object getImgTableVar( String key );

    /**
     * Get image number from the active image table.
     * @param key  active image name
     * @return   active image number corresponding to the given image name
     */
    public Object getActiveImgTableVar( String key );

    /**
     * Register the active image name in the active image table
     * @param key  active image name
     */
    public void putActiveVar( String key );

    /**
    * Changes the key associated with an active image variable, solely for use with
    * a "ChangeName" call in a script (changing the image name)
    * @param oldKey String old image key
    * @param newKey String new image key
    */
    public void changeActiveVar( String oldKey, String newKey);

    /**
     * Returns the variable name associated with a voi name. Like getVar, only for voi's.
     * @param key  the voi name
     * @return     The variable name, such as $voi1
     */
    public Object getVoiVar( String key );

    /**
     * Stores a voi name as a key to a voi variable. Like putVar, only for voi's.
     * @param key  The voi name to be associated with a new variable.
     */
    public void putVoiVar( String key );

    /**
     * Whether the script is working on the active image or a named image.
     * Set active image flag to true when script start recording.
     * @param flag  the active image flag
     */
    public void setActiveImageFlag( boolean flag );

    /**
     * Get active image flag (whether the script is working on the active image or a named image)..
     * @return   <code>true</code> if , <code>false</code> if not
     */
    public boolean getActiveImageFlag();
}
