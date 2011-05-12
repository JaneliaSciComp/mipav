package gov.nih.mipav.model.structures.event;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.util.*;


/**
 * An event from a VOIVector. Registers the VOIVector as the event source and, if needed, the VOI that changed within
 * the VOIVector.
 *
 * @version  Aug 2002
 * @author   David Parsons
 *
 *           <p>$Logfile: /mipav/src/gov/nih/mipav/model/structures/EVENT/VOIVectorEvent.java $ $Revision: 1 $ $Date:
 *           9/04/02 3:20p $</p>
 */
public class VOIVectorEvent extends EventObject {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2033500015284877824L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * holds reference because referring to a VOI held by the ViewVOIVector would cause all kinds of memory clean-up
     * concerns. may be null.
     */
    VOI mostRecentUpdate;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * creates an event with no VOI as reference.
     *
     * @param  eventSource  DOCUMENT ME!
     */
    public VOIVectorEvent(ViewVOIVector eventSource) {
        this(eventSource, null);
    }

    /**
     * creates an event using the supplied VOI as reference.
     *
     * @param  eventSource  DOCUMENT ME!
     * @param  changedVOI   DOCUMENT ME!
     */
    public VOIVectorEvent(ViewVOIVector eventSource, VOI changedVOI) {
        super(eventSource);
        mostRecentUpdate = changedVOI;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * cleanup. Sets the reference VOI to null, to prevent memory errors when the VOI itself is through.
     */
    public void finalize() {
        mostRecentUpdate = null;
    }

    /**
     * if the VOIVector were to have state, this would return it. currently returns <code>true</code>
     *
     * @return  DOCUMENT ME!
     */
    public boolean getState() {
        return true; // return ((ViewVOIVector) getSource()).getState();
    }

    /**
     * returns the reference VOI. May be <code>null</code>
     *
     * @return  DOCUMENT ME!
     */
    public VOI getVOI() {
        return mostRecentUpdate;
    }
}
