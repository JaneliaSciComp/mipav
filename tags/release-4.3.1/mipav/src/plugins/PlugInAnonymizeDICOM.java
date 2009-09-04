import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.view.*;

import java.awt.*;


public class PlugInAnonymizeDICOM implements PlugInGeneric{
	
	public void run() {
		new PlugInDialogAnonymizeDICOM(true);
	}

}
