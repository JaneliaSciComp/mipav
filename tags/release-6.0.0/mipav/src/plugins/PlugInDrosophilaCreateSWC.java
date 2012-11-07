import gov.nih.mipav.plugins.PlugInGeneric;


public class PlugInDrosophilaCreateSWC implements PlugInGeneric {

	@Override
	public void run() {
		new PlugInDialogDrosophilaCreateSWC(false);

	}

}
