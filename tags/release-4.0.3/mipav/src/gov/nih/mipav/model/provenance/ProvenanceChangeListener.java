package gov.nih.mipav.model.provenance;

import java.util.EventListener;


/**
 * Used by JDialogDataProvenance to display changes in the provenanceholder (when entries are added)
 *
 */
public interface ProvenanceChangeListener extends EventListener {

	/**
	 * 
	 * @param e the provenance change event
	 */
	public void provenanceStateChanged(ProvenanceChangeEvent e);

}