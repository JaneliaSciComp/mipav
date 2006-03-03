package gov.nih.mipav.model.structures;

import java.util.EventObject;

// update VOI event
/** presents an event to regard a changed VOI.
 *  Carries with it the changed VOIBase (the 
 *  changed curve associated with this VOI, 
 *  and should be <i>in</i> that 
 *  <code>VOI</code>), should the object announcing 
 *  this change need to include that information. 
 *  <p>
 *  Future versions may include other information 
 *  concerning the changed field of the Volume of 
 *  Interest, itself.
 *
 *       $Logfile: /mipav/src/gov/nih/mipav/model/structures/UpdateVOIEvent.java $
 *       $Revision: 1 $
 *       $Date: 8/08/02 10:05a $
 */
public class UpdateVOIEvent extends EventObject 
{
    private Object  source;
    private VOI     voi;
    private VOIBase curve;
    private boolean usedCurve = false;
    
    /** A basic updateVOIevent.  No information about 
     *  the VOI updated can be taken if event is created 
     *  with this creator.
     */
    public UpdateVOIEvent(Object src) {
        this(src, null);
    }
    
    /** 
    */
    public UpdateVOIEvent(Object src, VOI changedVOI) {
        this(src, changedVOI, null);
    }
    
    public UpdateVOIEvent(Object src, VOI changedVOI, VOIBase changedCurve) {
        super(src);
        source = src;
        voi = changedVOI;
        curve = changedCurve;
        // curve only has meaning if we actually 
        // set the curve to something non null
        usedCurve = (curve == null)? false : true;
    }
    
    public VOI getChangedVolumeOfInterest() {
        return voi;
    }
    
    public VOIBase getChangedCurveOfInterest() {
        if (!usedCurve) {
            return null;
        }
        return curve;
    }
    
    public String toString() {
        return voi.toString();
    }
    
    public Object getSource() {
        return source;
    }
    
}