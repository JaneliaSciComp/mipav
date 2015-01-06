import gov.nih.mipav.plugins.PlugInGeneric;


public class PlugInSWCVolume implements PlugInGeneric {

	@Override
	public void run() {
		try{
			new PlugInDialogSWCVolume();
		}catch(Exception e){
			e.printStackTrace();
		}
	}

}
