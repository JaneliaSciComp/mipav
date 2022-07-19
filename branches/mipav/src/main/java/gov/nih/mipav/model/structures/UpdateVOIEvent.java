package gov.nih.mipav.model.structures;


import java.util.*;


// update VOI event
/**
 * presents an event to regard a changed VOI. Carries with it the changed VOIBase (the changed curve associated with
 * this VOI, and should be <i>in</i> that <code>VOI</code>), should the object announcing this change need to include
 * that information.
 *
 * <p>Future versions may include other information concerning the changed field of the Volume of Interest, itself.</p>
 *
 * <p>$Logfile: /mipav/src/gov/nih/mipav/model/structures/UpdateVOIEvent.java $ $Revision: 1 $ $Date: 8/08/02 10:05a $
 * </p>
 */
public class UpdateVOIEvent extends EventObject {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 8233713497226707057L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private VOIBase curve;

    /** DOCUMENT ME! */
    private Object source;

    /** DOCUMENT ME! */
    private boolean usedCurve = false;

    /** DOCUMENT ME! */
    private VOI voi;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * A basic updateVOIevent. No information about the VOI updated can be taken if event is created with this creator.
     *
     * @param  src  DOCUMENT ME!
     */
    public UpdateVOIEvent(Object src) {
        this(src, null);
    }

    /**
     * Creates a new UpdateVOIEvent object.
     *
     * @param  src         DOCUMENT ME!
     * @param  changedVOI  DOCUMENT ME!
     */
    public UpdateVOIEvent(Object src, VOI changedVOI) {
        this(src, changedVOI, null);
    }

    /**
     * Creates a new UpdateVOIEvent object.
     *
     * @param  src           DOCUMENT ME!
     * @param  changedVOI    DOCUMENT ME!
     * @param  changedCurve  DOCUMENT ME!
     */
    public UpdateVOIEvent(Object src, VOI changedVOI, VOIBase changedCurve) {
        super(src);
        source = src;
        voi = changedVOI;
        curve = changedCurve;

        // curve only has meaning if we actually
        // set the curve to something non null
        usedCurve = (curve == null) ? false : true;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public VOIBase getChangedCurveOfInterest() {

        if (!usedCurve) {
            return null;
        }

        return curve;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public VOI getChangedVolumeOfInterest() {
        return voi;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Object getSource() {
        return source;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String toString() {
        return voi.toString();
    }

}
