import gov.nih.mipav.plugins.PlugInGeneric;


public class PlugInVolumeRender implements PlugInGeneric
{
	public void run()
	{
		new PlugInDialogVolumeRender(false);
	}

}
