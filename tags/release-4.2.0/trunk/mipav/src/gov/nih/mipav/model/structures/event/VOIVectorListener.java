package gov.nih.mipav.model.structures.event;


import java.util.*;


// VOI event change
/**
 * <code>VOISelectionListener</code> permits objects to listen for updates to a <code>VOI</code>. This may permit an
 * object to update its view when the <code>VOI</code> changes.
 *
 * @see  VOIEvent $Logfile: /mipav/src/gov/nih/mipav/model/structures/EVENT/VOIVectorListener.java $ $Revision: 1 $
 *       $Date: 9/04/02 3:20p $
 */
public interface VOIVectorListener extends EventListener {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * handles an VOIEvent as a selection change.
     *
     * @param  newVOIselection  DOCUMENT ME!
     */
    void addedVOI(VOIVectorEvent newVOIselection);

    /**
     * handles to the VOI being removed from the Vector.
     *
     * @param  removed  DOCUMENT ME!
     */
    void removedVOI(VOIVectorEvent removed);

    /**
     * handles the VOI being selected.
     *
     * @param  selection  DOCUMENT ME!
     */
    void vectorSelected(VOIVectorEvent selection);

}
