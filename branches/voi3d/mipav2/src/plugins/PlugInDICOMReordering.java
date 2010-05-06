import gov.nih.mipav.plugins.PlugInGeneric;

/**
 * @author pandyan
 * 
 * The plugin is used for sorting DICOM image slices based on the
 * patient location <0020,0037>
 * 
 * New dirs are created using patientID info as well as studyID and series...as well as whether the initial images were
 * in a "pre" or "post" folder
 *
 */
public class PlugInDICOMReordering implements PlugInGeneric {
	
	
	/**
	 * construcor
	 *
	 */
	public PlugInDICOMReordering() {
		
	}
 
	/**
	 * run
	 */
	public void run() {
		new PlugInDialogDICOMReordering(false);

	}

}
