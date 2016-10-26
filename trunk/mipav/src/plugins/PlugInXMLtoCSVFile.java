import gov.nih.mipav.plugins.PlugInGeneric;


public class PlugInXMLtoCSVFile implements PlugInGeneric {

	@Override
	public void run() {
		new PlugInDialogXMLtoCSVFile(false);

	}

}
