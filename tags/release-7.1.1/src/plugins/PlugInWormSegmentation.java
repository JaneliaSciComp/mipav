import gov.nih.mipav.plugins.PlugInGeneric;


public class PlugInWormSegmentation implements PlugInGeneric
{
	public void run()
	{
		new PlugInDialogWormSegmentation(false);
	}

}
