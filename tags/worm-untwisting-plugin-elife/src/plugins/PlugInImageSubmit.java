
import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

public class PlugInImageSubmit implements PlugInGeneric {
	public void run() {
		new PlugInDialogImageSubmit(false);
	}
}
