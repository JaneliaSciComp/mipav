import gov.nih.mipav.plugins.PlugInGeneric;


public class PlugIn3DSWCStats implements PlugInGeneric {

	@Override
	public void run() {
		// TODO Auto-generated method stub
		try{
			new PlugInDialog3DSWCStats();
		} catch (Exception e){
			e.printStackTrace();
		}
	}

}
