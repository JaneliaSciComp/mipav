package gov.nih.mipav.model.structures.event;


import gov.nih.mipav.model.structures.*;

import java.util.*;


/**
 * An event from a VOI. Registers the VOI as the event source and, if needed, the VOIBase that changed within the VOI.
 *
 * <p>It might be useful to have several different listeners VOI objects, to listen for different varieties of VOI
 * changes, say, for curve-changing, or property-changing.</p>
 *
 * <p>Perhaps in this case, the VOIListener could listen for all those, but most objects interested in the VOI, would
 * only listen for particular changes in the VOI.</p>
 *
 * @version  Aug 2002
 * @author   David Parsons
 * @author   Paul F. Hemler
 *
 *           <p>$Logfile: /mipav/src/gov/nih/mipav/model/structures/event/VOIEvent.java $ $Revision: 2 $ $Date: 04-11-23
 *           1:28p $</p>
 */
public class VOIEvent extends EventObject {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5542473955796520517L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * holds reference because referring to a VOI held by the ViewVOIVector would cause all kinds of memory clean-up
     * concerns. may be null.
     */
    VOIBase mostRecentUpdate;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * creates an event with no VOI as reference.
     *
     * @param  eventSource  DOCUMENT ME!
     */
    public VOIEvent(VOI eventSource) {
        this(eventSource, null);
    }

    /**
     * creates an event using the supplied VOI as reference.
     *
     * @param  eventSource   DOCUMENT ME!
     * @param  changedCurve  DOCUMENT ME!
     */
    public VOIEvent(VOI eventSource, VOIBase changedCurve) {
        super(eventSource);
        mostRecentUpdate = changedCurve;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * cleanup. Sets the reference VOIBase to null, to prevent memory errors when the VOIBase itself is through.
     */
    public void finalize() {
        mostRecentUpdate = null;
    }

    /**
     * returns the reference VOI. May be <code>null</code>
     *
     * @return  DOCUMENT ME!
     */
    public VOIBase getBase() {
        return mostRecentUpdate;
    }

    /**
     * if the VOI were to have state, this would return it. currently returns <code>true</code>
     *
     * @return  DOCUMENT ME!
     */
    public boolean getState() {
        return ((VOI) getSource()).isActive();
    }
    
    public VOI getVOI()
    {
        return ((VOI) getSource());
    }
}
