package gov.nih.mipav.model.structures.event;

import java.util.EventListener;

// VOI event change
/**
 *  <code>VOISelectionListener</code> permits objects to 
 *  listen for updates to a <code>VOI</code>.  This may 
 *  permit an object to update its view when the 
 *  <code>VOI</code> changes.
 *  <p>
 *
 *  @see VOIEvent
 *       $Logfile: /mipav/src/gov/nih/mipav/model/structures/EVENT/VOIVectorListener.java $
 *       $Revision: 1 $
 *       $Date: 9/04/02 3:20p $
 */
public interface VOIVectorListener extends EventListener 
{
    /** handles an VOIEvent as a selection change. */
    public void addedVOI(VOIVectorEvent newVOIselection);
    /** handles to the VOI being removed from the Vector */
    public void removedVOI(VOIVectorEvent removed);
    /** handles the VOI being selected. */
    public void vectorSelected(VOIVectorEvent selection);
    
}
