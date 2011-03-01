import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.view.*;

import java.awt.*;
import java.util.*;


/**
* Brain subcortical analysis is running from the command line.   User types command line such as,
* >mipav -p PlugInBrainSubcorticalRegistration -inDir F:\BrainMRIData -outDir F:\BrainMRIData  
* -vs compare.txt -Reg LH LA RP	
* 
* -p indicates running as a plugin
* -inDir specify the input brain MRI images repository directory. 
* -outDir specify the output directory for registered images, color RGB comparison images, and statics report. 
* -Reg flag indicates which subcortical sections to do registration. 
*  
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
    
    /** Flag to check that command line plugin is Brain subcortical */
    private boolean subcorticalFlag = false;
    
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
		for ( i = 0; i < args.length; i++ ) {
			if ( args[i].equalsIgnoreCase("-p")) {
				if ( args[++i].equalsIgnoreCase("PlugInBrainSubcorticalRegistration") ) {
					subcorticalFlag = true;
				}
			} else if ( args[i].equalsIgnoreCase("-inDir")) {
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
			}
		}
		return args.length-1;
	}
	
    /** 
	 * Call the Brain subcortical dialog to run registration. 
	 */
	public void run() {
		
		if (subcorticalFlag) {
			ViewUserInterface.getReference().setAppFrameVisible(false);
			new PlugInDialogBrainSubcortical(ViewUserInterface.getReference()
					.getMainFrame(), inputDir, outputDir, caseCompareDir, regSection);
		}
		
	}
	
	public void run(Frame parentFrame, ModelImage image) { 
		
	}
	
	
}
