package gov.nih.mipav.model.structures.event;

import java.awt.*;
import java.util.*;


// VOI event change
/**
 * <code>VOIListener</code> permits objects to listen for updates to a <code>VOI</code>. This may permit an object to
 * update its view when the <code>VOI</code> changes.
 *
 * <p>It might be useful to have several different listeners VOI objects, to listen for different varieties of VOI
 * changes, say, for curve-changing, or property-changing.</p>
 *
 * <p>Perhaps in this case, the VOIListener could listen for all those, but most objects interested in the VOI, would
 * only listen for particular changes in the VOI.</p>
 *
 * @see  VOIEvent $Logfile: /mipav/src/gov/nih/mipav/model/structures/event/VOIListener.java $ $Revision: 5 $ $Date:
 *       04-11-23 1:52p $
 */
public interface VOIListener extends EventListener {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * handles an VOIEvent as a selection change.
     *
     * @param  added  DOCUMENT ME!
     */
    void addedCurve(VOIEvent added);

    /**
     * handles to the VOI being removed from the Vector.
     *
     * @param  removed  DOCUMENT ME!
     */
    void removedCurve(VOIEvent removed);

    /**
     * handles the VOI being selected. -- a state-change.
     *
     * @param  selection  DOCUMENT ME!
     */
    void selectedVOI(VOIEvent selection);
    
    void colorChanged(Color c);
}
