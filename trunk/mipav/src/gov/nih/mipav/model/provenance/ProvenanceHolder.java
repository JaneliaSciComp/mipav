package gov.nih.mipav.model.provenance;


import java.util.*;

import javax.swing.event.EventListenerList;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ProgressChangeEvent;
import gov.nih.mipav.view.ProgressChangeListener;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.model.structures.*;


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
    
    public void addElement(ProvenanceEntry entry) {
    	fireProvenanceStateChanged(entry);
    	super.addElement(entry);
    }
    
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
    
    public void addProvenanceChangeListener(ProvenanceChangeListener l) {
        listenerList.add(ProvenanceChangeListener.class, l);
    }
    
}
