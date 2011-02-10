package gov.nih.mipav.model.structures;


import java.util.*;


// VOI event change
/**
 * <code>UpdateVOISelectionListener</code> permits objects to listen for updates to a <code>VOI</code>. This may permit
 * an object to update its view when the <code>VOI</code> changes.
 *
 * @see  UpdateVOIEvent $Logfile: /mipav/src/gov/nih/mipav/model/structures/UpdateVOISelectionListener.java $ $Revision:
 *       1 $ $Date: 8/08/02 10:06a $
 */
public interface UpdateVOISelectionListener extends EventListener {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * handles an UpdateVOIEvent as a selection change.
     *
     * @param  newVOIselection  DOCUMENT ME!
     */
    void selectionChanged(UpdateVOIEvent newVOIselection);
}
