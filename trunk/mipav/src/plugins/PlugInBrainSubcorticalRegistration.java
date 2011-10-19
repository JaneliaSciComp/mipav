import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.view.*;

import java.awt.*;
import java.util.*;


/**
* Brain subcortical analysis is running from the command line.   User types command line such as,
* >mipav -p PlugInBrainSubcorticalRegistration -inDir F:\BrainMRIData -outDir F:\BrainMRIData  
* -vs F:\BrainMRIData\compare.txt -Reg LH LA RP	
* 
* -p PlugInBrainSubcorticalRegistration	  the mandate flag to indicate running as a plugin
* -inDir 	specify the input brain MRI images repository directory. 
* -outDir 	specify the output directory for registered images, color RGB comparison images, and statistic report. 
* -vs 		cases comparison text file
* -Reg 		flag indicates which subcortical sections to do registration. 
* -printHelp 	print help instruction. 
*
* Left Hippocampus  ---   LH
* Right Hippocampus ---   RH
* Left Amygdala     ---   LA
* Right Amygdala    ---   RA
* Left Candate      ---   LC
* Right Candate     ---   RC
* Left Putamen      ---   LP
* Right Putaman     ---   RP
* Left globus pallidess  --- LG
* Right globus pallidees --- RG
* Left Thalomus     ---   LT
* Right Thalomus    ---   RT 
*/
public class PlugInBrainSubcorticalRegistration implements PlugInGeneric, CommandLineParser {
	
	/** input directory specified by the command line */
	private String inputDir;
	
	/** output directory specified by the command line */
	private String outputDir;
	
	/** directory to store the case compare file. */ 
	private String caseCompareDir;
	
	/** Vector indicates which subsections to do registration. */
    private Vector<Integer> regSection = new Vector<Integer>();
    
    /** Constants for each subsections. */
    /**
	 *
	 * Left Hippocampus  ---   LH
	 * Right Hippocampus ---   RH
	 * Left Amygdala     ---   LA
	 * Right Amygdala    ---   RA
	 * Left Candate      ---   LC
	 * Right Candate     ---   RC
	 * Left Putamen      ---   LP
	 * Right Putaman     ---   RP
	 * Left globus pallidess  --- LG
	 * Right globus pallidees --- RG
	 * Left Thalomus     ---   LT
	 * Right Thalomus    ---   RT 
	 */
    public static int LH = 0; 
    public static int RH = 1; 
    public static int LA = 2; 
    public static int RA = 3; 
    public static int LC = 4; 
    public static int RC = 5; 
    public static int LP = 6; 
    public static int RP = 7; 
    public static int LG = 8; 
    public static int RG = 9; 
    public static int LT = 10; 
    public static int RT = 11;
   
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
			} else if ( args[i].equalsIgnoreCase("-vs")) {
				caseCompareDir = args[++i];
			} else if ( args[i].equalsIgnoreCase("-Reg")) {
				for ( int j = i+1; j < args.length; j++ ) {
					if ( args[j].equalsIgnoreCase("LH")) {
						regSection.add(LH);
					} else if ( args[j].equalsIgnoreCase("RH")) {
						regSection.add(RH);
					} else if ( args[j].equalsIgnoreCase("LA")) {
						regSection.add(LA);
					} else if ( args[j].equalsIgnoreCase("RA")) {
						regSection.add(RA);
					} else if ( args[j].equalsIgnoreCase("LC")) {
						regSection.add(LC);
					} else if ( args[j].equalsIgnoreCase("RC")) {
						regSection.add(RC);
					} else if ( args[j].equalsIgnoreCase("LP")) {
						regSection.add(LP);
					} else if ( args[j].equalsIgnoreCase("RP")) {
						regSection.add(RP);
					} else if ( args[j].equalsIgnoreCase("LG")) {
						regSection.add(LG);
					} else if ( args[j].equalsIgnoreCase("RG")) {
						regSection.add(RG);
					} else if ( args[j].equalsIgnoreCase("LT")) {
						regSection.add(LT);
					} else if ( args[j].equalsIgnoreCase("RT")) {
						regSection.add(RT);
					}
				}
				break;
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
    	System.out.println("> mipav -p PlugInBrainSubcorticalRegistration -inDir F:\\BrainMRIData -outDir F:\\BrainMRIData");  
    	System.out.println("-vs F:\\BrainMRIData\\compare.txt -Reg LH LA RP");	
    	System.out.println(""); 
    	System.out.println("[-p PlugInBrainSubcorticalRegistration]	 the mandate flag to indicate running as a plugin");
    	System.out.println("[-inDir] 		specify the input brain MRI images repository directory."); 
    	System.out.println("[-outDir] 		specify the output directory for registered images, color RGB comparison images, and statistic report."); 
    	System.out.println("[-vs] 			cases comparison text file");
    	System.out.println("[-Reg] 			flag indicates which subcortical sections to do registration."); 
    	System.out.println("[-printHelp] 		print the help instruction");
    	System.out.println("");
    	System.out.println("Left Hippocampus  ---   LH");
    	System.out.println("Right Hippocampus ---   RH");
    	System.out.println("Left Amygdala     ---   LA");
    	System.out.println("Right Amygdala    ---   RA");
    	System.out.println("Left Candate      ---   LC");
    	System.out.println("Right Candate     ---   RC");
    	System.out.println("Left Putamen      ---   LP");
    	System.out.println("Right Putaman     ---   RP");
    	System.out.println("Left globus pallidess  --- LG");
    	System.out.println("Right globus pallidees --- RG");
    	System.out.println("Left Thalomus     ---   LT");
    	System.out.println("Right Thalomus    ---   RT"); 
        System.out.println("");
    	System.out.println("compare.txt  contents:");
    	System.out.println("03949 vs 03556");
    	System.out.println("03555 vs 03556");
    	System.out.println("03555 vs 03949");
    }
    
    /** 
	 * Call the Brain subcortical dialog to run registration. 
	 */
	public void run() {
		ViewUserInterface.getReference().setAppFrameVisible(false);
		new PlugInDialogBrainSubcortical(ViewUserInterface.getReference()
				.getMainFrame(), inputDir, outputDir, caseCompareDir, regSection);
	}
	
	public void run(Frame parentFrame, ModelImage image) { 
		
	}
	
	
}
