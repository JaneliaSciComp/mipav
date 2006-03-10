package gov.nih.mipav.model.structures.event;

import java.util.EventObject;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.view.ViewVOIVector;

/** An event from a VOIVector.  Registers the VOIVector as the 
*   event source and, if needed, the VOI that changed within 
*   the VOIVector.  
*   
*		@version    Aug 2002
*		@author     David Parsons
*
*       $Logfile: /mipav/src/gov/nih/mipav/model/structures/EVENT/VOIVectorEvent.java $
*       $Revision: 1 $
*       $Date: 9/04/02 3:20p $
*
*/
public class VOIVectorEvent extends EventObject {
    // holds reference because referring to a VOI held by the 
    // ViewVOIVector would cause all kinds of memory clean-up 
    // concerns.  may be null.
    VOI mostRecentUpdate;

    /** creates an event with no VOI as reference */
    public VOIVectorEvent(ViewVOIVector eventSource) {
        this(eventSource, null);
    }

    /** creates an event using the supplied VOI as reference */
    public VOIVectorEvent(ViewVOIVector eventSource, VOI changedVOI) {
        super(eventSource);
        mostRecentUpdate = changedVOI;
    }
    
    /** returns the reference VOI.  May be <code>null</code> */
    public VOI getVOI() {
        return mostRecentUpdate;
    }
    
    /** if the VOIVector were to have state, this would return it.  
    *   currently returns <code>true</code>
    */
    public boolean getState() {
        return true;//return ((ViewVOIVector) getSource()).getState();
    }
    
    /** cleanup.  Sets the reference VOI to null, to prevent 
    *   memory errors when the VOI itself is through.
    */
    public void finalize() {
        mostRecentUpdate = null;
    }
}