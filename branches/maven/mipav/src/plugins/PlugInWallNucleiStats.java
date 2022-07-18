import gov.nih.mipav.plugins.PlugInGeneric;


public class PlugInWallNucleiStats implements PlugInGeneric {

	public static final String[] CATEGORY = {"Generic"};
	
	@Override
	public void run() {
		// TODO Auto-generated method stub
		new PlugInDialogWallNucleiStats();
	}

}
