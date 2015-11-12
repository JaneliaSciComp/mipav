import gov.nih.mipav.plugins.PlugInGeneric;


public class PlugInFlattenCSVFile implements PlugInGeneric {

	@Override
	public void run() {
		new PlugInDialogFlattenCSVFile(false);

	}

}
