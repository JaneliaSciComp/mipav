import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.view.*;

import java.awt.*;
import java.util.*;


/**
*  This class generates the knees VOI contours from the deep learning HED MRI maps.
*  10-fold cross-validataion scenario. 
*  User specifies the command line such as,
* > mipav -p PlugInSK10_MRI_map_nopre -inDirImage /backup/Knee_2010_challenge/train_data_no_pre/axial/ 
* -inDirMap /backup/Knee_2010_challenge/test_out_no_pre/fold1/axial/ 
* -outDir /project/DeepLearning/backup/Knee_2010_challenge/train_img_sk10_femur_cart/result_voi/fold1  
* 
* -p PlugInSK10_MRI_map_pre	  the mandate flag to indicate running as a plugin
* -inDirImage 	specify the input testing images directory.  10-fold cross-validataion scenario. 
* -inDirMap 	specify the HNN MRI images prediction maps directory. 
* -outDir 	specify the output directory generated VOIs and corresonding testing image. 
* -printHelp 	print help instruction. 
*
* @author Ruida Cheng
*/
public class PlugInSK10_MRI_map_nopre implements PlugInGeneric, CommandLineParser {
	
	/** input directory specified by the command line */
	private String inputDirImage = "/home/ruida/Knee_2010_challenge/train_data_no_pre/axial/";
	
	/** input directory specified by the command line */
	private String inputDirMap = "/home/ruida/Knee_2010_challenge/test_out_no_pre/fold3/axial/";
	
	/** output directory specified by the command line */
	private String outputDir = "/home/ruida/Knee_2010_challenge/testout_ski10_nopre";
	
   
    /**
     * Command line processing
     */
    public int parseArguments(final String[] args, final int initArg) {
		int i;
		for ( i = initArg; i < args.length; i++ ) {
			if ( args[i].equalsIgnoreCase("-inDirImage")) {
				inputDirImage = args[++i];
			} else if ( args[i].equalsIgnoreCase("-inDirMap")) {
				inputDirMap = args[++i];
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
    	System.out.println("> mipav -p PlugInSK10_MRI_map_nopre -inDirImage /home/ruida/Knee_2010_challenge/train_data_no_pre/axial/ \n" + 
    			"-inDirMap /home/ruida/Knee_2010_challenge/test_out_no_pre/fold3/axial/ \n" + 
    			"-outDir \"/home/ruida/Knee_2010_challenge/testout_ski10_nopre");	
    	System.out.println(""); 
    	System.out.println("[-p PlugInSK10_MRI_map_pre]	 the mandate flag to indicate running as a plugin");
    	System.out.println("[-inDirImage] 		specify the input testing images directory.  10-fold cross-validataion scenario."); 
    	System.out.println("[-inDirMap] 		specify the HNN MRI images prediction maps directory.");
    	System.out.println("[-outDir] 		specify the output directory generated VOIs and corresonding testing image."); 
    	System.out.println("[-printHelp] 		print the help instruction");
    	
    }
    
    /** 
	 * Call the Brain subcortical dialog to run registration. 
	 */
	public void run() {
		ViewUserInterface.getReference().setAppFrameVisible(false);
		new PluginDialogSK10_MRI_map_nopre(ViewUserInterface.getReference()
				.getMainFrame(), inputDirImage, inputDirMap, outputDir);
	}
	
	public void run(Frame parentFrame, ModelImage image) { 
		
	}
	
	
}
