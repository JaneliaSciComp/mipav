package gov.nih.mipav.model.structures.event;

import java.util.EventObject;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;

/** An event from a VOI.  Registers the VOI as the 
*   event source and, if needed, the VOIBase that changed 
*   within the VOI.  
*   <p>
*   It might be useful to have several different listeners 
*   VOI objects, to listen for different varieties of VOI 
*   changes, say, for curve-changing, or property-changing.  
*   <p>
*   Perhaps in this case, the VOIListener could listen for 
*   all those, but most objects interested in the VOI, would 
*   only listen for particular changes in the VOI.  
*  
*		@version    Aug 2002
*		@author     David Parsons
*               @author     Paul F. Hemler
*
*       $Logfile: /mipav/src/gov/nih/mipav/model/structures/event/VOIEvent.java $
*       $Revision: 2 $
*       $Date: 04-11-23 1:28p $
*   
*/
public class VOIEvent extends EventObject {
    // holds reference because referring to a VOI held by the 
    // ViewVOIVector would cause all kinds of memory clean-up 
    // concerns.  may be null.
    VOIBase mostRecentUpdate;
    
    /** creates an event with no VOI as reference */
    public VOIEvent(VOI eventSource) {
        this(eventSource, null);
    }
    
    /** creates an event using the supplied VOI as reference */
    public VOIEvent(VOI eventSource, VOIBase changedCurve) {
        super(eventSource);
        mostRecentUpdate = changedCurve;
    }
    
    /** returns the reference VOI.  May be <code>null</code> */
    public VOIBase getBase() {
        return mostRecentUpdate;
    }
    
    /** if the VOI were to have state, this would return it.  
    *   currently returns <code>true</code>
    */
    public boolean getState() {
        return ((VOI) getSource()).isActive();
    }
    
    /** cleanup.  Sets the reference VOIBase to null, to prevent 
    *   memory errors when the VOIBase itself is through.
    */
    public void finalize() {
        mostRecentUpdate = null;
    }
}
