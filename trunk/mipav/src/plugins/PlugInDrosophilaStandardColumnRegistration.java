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
		for(int i=0;i<args.length;i++) {
			String varName = args[i];
			i = i + 1;
			String value = args[i];
			if( varName.equals("image")) {
				
			}else if(varName.equals("pointsFile")) {
				
			}else if(varName.equals("filamentFile")) {
				
			}else if(varName.equals("filamentSamplin")) {
				
			}else if(varName.equals("flipX")) {
				
			}else if(varName.equals("flipY")) {
				
			}else if(varName.equals("flipZ")) {
				
			}
			
			
			
			
			
			
		}
		
		
		
		
		return 0;
	}

}
