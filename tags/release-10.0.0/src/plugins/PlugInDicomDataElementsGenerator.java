import gov.nih.mipav.plugins.PlugInGeneric;


public class PlugInDicomDataElementsGenerator implements PlugInGeneric {


	public void run() {
		new PlugInDialogDicomDataElementsGenerator(false);

	}

}
