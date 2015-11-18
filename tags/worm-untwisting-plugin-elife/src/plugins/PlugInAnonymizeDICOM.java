

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface






public class PlugInAnonymizeDICOM implements PlugInGeneric{
	
	public void run() {
		new PlugInDialogAnonymizeDICOM(true);
	}

}
