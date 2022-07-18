import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.view.*;

import java.awt.*;
import java.util.*;


/**
 * This file reads the SKI10 dataset, converts the 3D images and corresponding masks into 2D
 * png slices -- 10 folds cross-validation.  
 * 
 * no image preprocessing steps  
 * 
 * SKI10 dataset is given actually in sagittal view, however, the image orientation attribute 
 * of the SKI 10 dataset is axial.  We have to bare with it.    
 * 
 * User specifies the command line such as,
 * >mipav -p PlugInKneesFemurSegmentationSKI10_nopre -inDir /scratch/Knee_2010_challenge/trainData
 *  -outDir /scratch/Knee_2010_challenge/sk10_nopre  
 * 
 * -p PlugInKneesFemurSegmentationSKI10_nopre	  the mandate flag to indicate running as a plugin
 * -inDir 	specify the SKI10 training image directory 
 * -outDir 	specify the output directory for training 10 folds directory 
 * -printHelp 	print help instruction. 
 *
 * @author Ruida Cheng
 */
public class PlugInKneesFemurSegmentationSKI10_nopre implements PlugInGeneric, CommandLineParser {
	
	/** input directory specified by the command line */
	private String inputDir = "/scratch/Knee_2010_challenge/trainData";
	
	/** output directory specified by the command line */
	private String outputDir = "/scratch/Knee_2010_challenge/sk10_nopre";
	
   
   
    /**
     * Command line processing
     */
    public int parseArguments(final String[] args, final int initArg) {
		int i;
		for ( i = initArg; i < args.length; i++ ) {
			if ( args[i].equalsIgnoreCase("-inDir")) {
				inputDir = args[++i];
			} else if ( args[i].equalsIgnoreCase("-outDir")) {
				outputDir = args[++i];
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
    	System.out.println("> mipav -p PlugInKneesFemurSegmentationSKI10_nopre -inDir /scratch/Knee_2010_challenge/trainData\n" +
    			"-outDir /scratch/Knee_2010_challenge/sk10_nopre");	
    	System.out.println(""); 
    	System.out.println("[-p PlugInKneesFemurSegmentationSKI10_nopre]  the mandate flag to indicate running as a plugin");
    	System.out.println("[-inDir] 	specify the SKI10 training image directory"); 
    	System.out.println("[-outDir]   specify the output directory for training 10 folds directory."); 
    	
    	System.out.println("[-printHelp] 		print the help instruction");
    	
    }
    
    /** 
	 * Call the Brain subcortical dialog to run registration. 
	 */
	public void run() {
		ViewUserInterface.getReference().setAppFrameVisible(false);
		new PlugInDialogKneesFemurSegmentationSKI10_nopre(ViewUserInterface.getReference()
				.getMainFrame(), inputDir, outputDir);
	}
	
	public void run(Frame parentFrame, ModelImage image) { 
		
	}
	
	
}
