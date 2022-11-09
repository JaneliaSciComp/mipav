import gov.nih.mipav.plugins.PlugInGeneric;


public class PlugInPhenXToCDE implements PlugInGeneric {

	@Override
	public void run() {
		// TODO Auto-generated method stub
		
		new PlugInDialogPhenXToCDE();
		
		/*File dataFile = new File("D:\\PhenX\\PhenX_PTSD.csv");
		File idFile = new File("D:\\PhenX\\FormIDs.csv");
		
		PlugInAlgorithmPhenXToCDE alg = new PlugInAlgorithmPhenXToCDE(dataFile, idFile);
		
		alg.run();
		
		System.exit(0);*/
	}

}
