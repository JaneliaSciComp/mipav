import gov.nih.mipav.plugins.PlugInGeneric;


public class PlugInImageStitchingGUI implements PlugInGeneric {

	@Override
	public void run() {
		// TODO Auto-generated method stub
		try{
		new PlugInDialogImageStitchingGUI();
		}catch(Exception e){
			e.printStackTrace();
		}
		
	}

}
