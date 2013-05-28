package gov.nih.mipav.view.input.spacenav;

/**
 * Interface for receiving space navigator events.
 * 
 * @author justinsenseney
 */
public interface SpaceNavigatorListener {
	/** Processes a polled event from the space navigator */
	public void processSpaceNavEvent();
}
