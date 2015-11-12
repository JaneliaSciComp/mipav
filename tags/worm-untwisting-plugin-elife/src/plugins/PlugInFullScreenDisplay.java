import gov.nih.mipav.plugins.PlugInGeneric;


public class PlugInFullScreenDisplay implements PlugInGeneric {

	@Override
	public void run() {
		new PlugInDialogFullScreenDisplay(false);

	}

}
