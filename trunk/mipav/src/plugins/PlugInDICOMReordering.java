import gov.nih.mipav.plugins.PlugInGeneric;


public class PlugInDICOMReordering implements PlugInGeneric {
	
	public PlugInDICOMReordering() {
		
	}
 
	
	public void run() {
		new PlugInDialogDICOMReordering(false);

	}

}
