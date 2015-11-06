
import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

public class PlugInCodebookParse implements PlugInGeneric {
	public void run() {
		new PlugInDialogCodebookParse(false);
	}
}
