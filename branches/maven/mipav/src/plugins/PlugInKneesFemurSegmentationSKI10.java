import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.view.*;

import java.awt.*;
import java.util.*;


/**
* This file presents the 3D-orthogonal approach to segment the MRI knees femur from SKI10 dataset.   And it is one ablation 
 * experiment we conducted for MRM paper.   
 * 
 * This file pre-processes SKI10 dataset with iso-tropic resampling (0.5 mm x 0.5 mm x 0.5 mm), converting SKI10 axial images
 * to coronal and sagittal images, generating Coherence Enhanced Diffusion (CED) filter images from corresponding MRI images. 
 * Then convert the 3D images to 2D png slices for training and testing. 10-fold cross-validation is used for validating the
 * experiments. 
 * 
 * !!!!!!!!!!!!   Strongly suggest that you use large memory to processing this step.  i.e. 128 GB memory !!!!!!!!!!!!!!!
 * 
 *  SKI 10 dataset is given actually in sagittal view, however, the image attribute of the SKI 10 dataset is axial.  We 
 *  have to bare with it.    
 *  
 *  This file creates the 10-fold cross-validation.  The file saves the corresponding png 2D slice files for each fold.  In order
 *  to distinguish the the training fold and testing fold.  We will use shell script to isolate them.  i.e. for fold 1, the rest
 *  folds from 2 to 10 compose the training fold for fold 1.   
 * 
 * User specifies the command line such as,
 * >mipav -p PlugInKneesFemurSegmentationSKI10 -inDir /scratch/Knee_2010_challenge/trainData
 *  -outDir /scratch/Knee_2010_challenge/sk10_pre  
 * 
 * -p PlugInKneesFemurSegmentationSKI10	  the mandate flag to indicate running as a plugin
 * -inDir 	specify the SKI10 training image directory 
 * -outDir 	specify the output directory for training 10 folds directory 
 * -printHelp 	print help instruction. 
 *
 * @author Ruida Cheng
 */

public class PlugInKneesFemurSegmentationSKI10 implements PlugInGeneric, CommandLineParser {
	
	/** input directory specified by the command line */
	private String inputDir = "/scratch/Knee_2010_challenge/trainData";
	
	/** output directory specified by the command line */
	private String outputDir = "/scratch/Knee_2010_challenge/sk10_pre";
	
   
   
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
    	System.out.println("mipav -p PlugInKneesFemurSegmentationSKI10 -inDir /scratch/Knee_2010_challenge/trainData\n" +
    			"-outDir /scratch/Knee_2010_challenge/sk10_pre");	
    	System.out.println(""); 
    	System.out.println("[-p PlugInKneesFemurSegmentationSKI10]  the mandate flag to indicate running as a plugin");
    	System.out.println("[-inDir] 	specify the SKI10 training image directory"); 
    	System.out.println("[-outDir]   specify the output directory for training 10 folds directory."); 
    	
    	System.out.println("[-printHelp] 		print the help instruction");
    	
    }
    
    /** 
	 * Call the Brain subcortical dialog to run registration. 
	 */
	public void run() {
		ViewUserInterface.getReference().setAppFrameVisible(false);
		new PlugInDialogKneesFemurSegmentationSKI10(ViewUserInterface.getReference()
				.getMainFrame(), inputDir, outputDir);
	}
	
	public void run(Frame parentFrame, ModelImage image) { 
		
	}
	
	
}
