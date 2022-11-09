import gov.nih.mipav.plugins.PlugInGeneric;


public class PlugInCellFiring implements PlugInGeneric {

	@Override
	public void run() {
		new PlugInDialogCellFiring(false);

	}

}
