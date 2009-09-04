package gov.nih.mipav.model.provenance;


import java.util.*;

import javax.swing.event.EventListenerList;


/**
 * Class that stores matrices (owned by ModelImage) with accessor functions to the matrix map/adding and changing
 * matrices.
 *
 * @author  linkb
 */
public class ProvenanceHolder extends Vector<ProvenanceEntry> {

    //~ Instance fields ------------------------------------------------------------------------------------------------
 
	/** A list of the ChangeListeners which are interested in the ChangeEvent. */
    private EventListenerList listenerList = new EventListenerList();
	
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor.
     *
     * @param  nDims  dimensions of image
     */
    public ProvenanceHolder() {
    }

    //~ Methods --------------------------------------------------------------------------------------------------------
    
    /**
     * Copies the object.
     *
     * @return  Object A copy of the file info.
     */
    public Object clone() {
        Object base = super.clone();

        return (base);
    }
    
    /**
     * Catches the Vector's addElement for listener notifaction
     */
    public void addElement(ProvenanceEntry entry) {
    	fireProvenanceStateChanged(entry);
    	super.addElement(entry);
    }
    
    /**
     * Notify the listeners (dialogs) that a provenance entry has been stored
     * @param entry the provenance entry
     */
    private void fireProvenanceStateChanged(ProvenanceEntry entry) {
    	Object[] listeners = listenerList.getListenerList();

        if (listeners == null) {
            return;
        }
        
        for (int i = listeners.length - 2; i >= 0; i -= 2) {
            if (listeners[i] == ProvenanceChangeListener.class) {
                ProvenanceChangeEvent event = new ProvenanceChangeEvent(this, entry);
                ((ProvenanceChangeListener) listeners[i + 1]).provenanceStateChanged(event);
            }
        }
    }
    
    /**
     * Adds a listener (JDialogDataProvenance) to receive notifications
     * @param l
     */
    public void addProvenanceChangeListener(ProvenanceChangeListener l) {
        listenerList.add(ProvenanceChangeListener.class, l);
    }
    
}
