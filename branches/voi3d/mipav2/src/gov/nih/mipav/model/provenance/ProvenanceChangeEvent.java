package gov.nih.mipav.model.provenance;

import javax.swing.event.ChangeEvent;

/**
 * 
 * Provenance Change event for listeners to update (jtable provenance viewing)
 *  
 */
public class ProvenanceChangeEvent extends ChangeEvent{
   
	private ProvenanceEntry entry;

    /**
     * default constructor
     * @param source the source of the event
     * @param entry the provenance entry
     */
	public ProvenanceChangeEvent(Object source, ProvenanceEntry entry){
	    super(source);
	    this.entry = entry;
	}

	/**
	 * Retrieves the provenance entry
	 * @return the entry
	 */
	public ProvenanceEntry getEntry(){
		return entry;
	}


}