package org.janelia.mipav.plugins.worm.untwisting;

/**
 * The listener interface for receiving changes to accurateMode.
 * The class that is interested in processing an action event
 * implements this interface, and the object created with that
 * class is registered with a component, using the component's
 * {@code addAccurateModeListener} method. When the action event
 * occurs, that object's {@code accurateModeChanged} method is
 * invoked.
 * 
 * @author diyi chen
 */
public interface AccurateModeListener {
	/**
	 * Add this method for responding to the accurateModeListener
	 * @param isAccurateMode true if the state of the mode is accurate
	 */
    void accurateModeChanged(boolean isAccurateMode);
}