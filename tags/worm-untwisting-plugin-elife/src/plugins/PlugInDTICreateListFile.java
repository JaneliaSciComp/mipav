import gov.nih.mipav.plugins.PlugInGeneric;


public class PlugInDTICreateListFile implements PlugInGeneric {
	
	public PlugInDTICreateListFile() {
		
	}

	public void run() {
		new PlugInDialogDTICreateListFile(false);

	}

}
