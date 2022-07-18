import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.view.*;

import java.awt.*;


/**
 * The class converts the 2D-volumetric approach axial, sagittal and coronal MRI and CED png slices (or MRI png slices alone)
 * into a file list.  HED uses the list to train HED deep learning model. 
 * 
 * User specifies the command line such as,
 * >mipav -p PlugInKneeTrainingListGenerator -inDir /project/DeepLearning/backup/Knee_2010_challenge/train_img_sk10_femur_cart/train_folds/  
 * 
 * -p PlugInKneeTrainingListGenerator	  the mandate flag to indicate running as a plugin
 * -inDir 	specify the 10-fold cross-validation training fold dirctory.  
 * -printHelp 	print help instruction. 
 *
 * @author Ruida Cheng
 */
public class PlugInKneeTrainingListGenerator implements PlugInGeneric, CommandLineParser {
	
	/** input directory specified by the command line */
	private String inputDir = "/project/DeepLearning/backup/Knee_2010_challenge/train_img_sk10_femur_cart/train_folds/";
	
   
    /**
     * Command line processing
     */
    public int parseArguments(final String[] args, final int initArg) {
		int i;
		for ( i = initArg; i < args.length; i++ ) {
			if ( args[i].equalsIgnoreCase("-inDir")) {
				inputDir = args[++i];
			} else if ( args[i].equalsIgnoreCase("-outDir")) {
	
			} else if ( args[i].equalsIgnoreCase("-printHelp")) {
				printHelpInfo();
				System.exit(0);
			} else {
				printHelpInfo();
				System.exit(0);
			}
		}
		return args.length-1;
	}
	
    /**
     * Print the help information.
     */
    public void printHelpInfo() {
    	System.out.println("Examples :");
    	System.out.println(">mipav -p PlugInKneeTrainingListGenerator -inDir /project/DeepLearning/backup/Knee_2010_challenge/train_img_sk10_femur_cart/train_folds/  \n" + 
    			" * ");	
    	System.out.println(""); 
    	System.out.println("[-p PlugInKneeTrainingListGenerator]  the mandate flag to indicate running as a plugin");
    	System.out.println("[-inDir] 	specify the 10-fold cross-validation training fold dirctory");  
    	System.out.println("[-printHelp] 		print the help instruction");
    	
    }
    
	public void run() {
		ViewUserInterface.getReference().setAppFrameVisible(false);
		new PlugInDialogKneesTrainingListGenerator(ViewUserInterface.getReference().getMainFrame(), inputDir);
	}
	
	public void run(Frame parentFrame, ModelImage image) { 
		
	}
	
	
}
