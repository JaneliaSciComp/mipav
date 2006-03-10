package gov.nih.mipav.model.structures;

import java.util.EventListener;

// VOI event change
/**
 *  <code>UpdateVOISelectionListener</code> permits objects to 
 *  listen for updates to a <code>VOI</code>.  This may 
 *  permit an object to update its view when the 
 *  <code>VOI</code> changes.
 *  <p>
 *
 *  @see UpdateVOIEvent
  *       $Logfile: /mipav/src/gov/nih/mipav/model/structures/UpdateVOISelectionListener.java $
 *       $Revision: 1 $
 *       $Date: 8/08/02 10:06a $
 */
public interface UpdateVOISelectionListener extends EventListener 
{
    /** handles an UpdateVOIEvent as a selection change. */
    public void selectionChanged(UpdateVOIEvent newVOIselection);
}
