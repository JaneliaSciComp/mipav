package gov.nih.mipav.view;

import java.util.EventListener;

/**
 * Defines an object which listens for the ProgressChangeEvent.
 * 
 * @author Hailong Wang, Ph.D
 * @version 1.0
 * @created 30-Jun-2006 9:27:52 AM
 */
public interface ProgressChangeListener extends EventListener {

	/**
	 * Invoked when the target of the listener has changed its state
	 * @param e  an ProgressChangeEvent object.
	 */
	public void progressStateChanged(ProgressChangeEvent e);

}