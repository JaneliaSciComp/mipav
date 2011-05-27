import java.awt.GraphicsEnvironment;

import gov.nih.mipav.plugins.PlugInGeneric;
import gov.nih.mipav.view.CommandLineParser;


public class PlugInDrosophilaStandardColumnRegistration implements PlugInGeneric, CommandLineParser {

	public PlugInDrosophilaStandardColumnRegistration() {
		
	}


	
	public void run() {
		
		/*if(GraphicsEnvironment.isHeadless()) {
			
		}*/
		
		new PlugInDialogDrosophilaStandardColumnRegistration(false);

	}

	
	public int parseArguments(String[] args, int initArg) {
		// TODO Auto-generated method stub
		return 0;
	}

}
